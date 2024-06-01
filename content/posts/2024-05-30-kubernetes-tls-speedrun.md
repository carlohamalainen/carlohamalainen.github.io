---
date: 2024-05-30T10:06:56+08:00
title: Kubernetes TLS minimal configuration speedrun
author: "Carlo Hamalainen"
url: /2024/05/30/2024-05-30-kubernetes-tls-speedrun
---

The goal is to deploy automatic TLS certificates using [cert-manager](https://cert-manager.io/docs), with certificates supplied by
[Let's Encrypt](https://letsencrypt.org/).

I've tried to produce a minimal set of files from [Digital Ocean's Kubernetes Starter Kit - 03 nginx](https://github.com/digitalocean/Kubernetes-Starter-Kit-Developers/blob/main/03-setup-ingress-controller/nginx.md). I had trouble with that guide due to some strange timeout issues; bumping the version of cert-manager seemed to help (see my [PR](https://github.com/digitalocean/Kubernetes-Starter-Kit-Developers/pull/208)). I also changed to a ``cluster-issuer`` instead of an ``issuer`` for cert-manager.

# Kubernetes setup

I'll assume you have a functioning Kubernetes cluster, for example a basic 2-node cluster from <https://www.digitalocean.com/products/kubernetes>.

Make sure that ``kubectl`` commands work.

# Clone the minimal example repo

To follow along, clone [this repo](https://github.com/carlohamalainen/minimal-kubernetes-tls-digital-ocean):

```shell
$ git clone https://github.com/carlohamalainen/minimal-kubernetes-tls-digital-ocean.git
$ cd minimal-kubernetes-tls-digital-ocean
$ ls | sort
README.md
clusterissuer-letsencrypt-nginx.yaml
nginx-values-v4.1.3.yaml
quote_deployment.yaml
quote_host.yaml
quote_service.yaml
```

# Install ingress-nginx

Install [ingress-nginx](https://kubernetes.github.io/ingress-nginx):

```shell
helm repo add ingress-nginx https://kubernetes.github.io/ingress-nginx
helm repo update ingress-nginx

NGINX_CHART_VERSION="4.1.3"

helm install ingress-nginx ingress-nginx/ingress-nginx --version "$NGINX_CHART_VERSION" \
  --namespace ingress-nginx \
  --create-namespace \
  -f "nginx-values-v${NGINX_CHART_VERSION}.yaml"
```

# Create a DNS record

This is the one somewhat manual step - you need to point your domain name to the load balancer's ``EXTERNAL-IP``:

```shell-session
$ kubectl get svc -n ingress-nginx
NAME                                 TYPE           CLUSTER-IP       EXTERNAL-IP       PORT(S)                      AGE
ingress-nginx-controller             LoadBalancer   10.245.164.122   111.222.333.444   80:31793/TCP,443:32230/TCP   35h
ingress-nginx-controller-admission   ClusterIP      10.245.157.142   <none>            443/TCP                      35h
```

Add an ``A`` record with your DNS provider so that ``api.example.com`` points to this IP.

For testing, it's helpful to set the TTL to 5 minutes so that you don't have to wait 3600 seconds (the default) for DNS to update.

# Deploy the quote service

The [datawire/quote](https://github.com/datawire/quote) service exposes a REST API. Here is its configuration file:

The quote deployment in ``quote_deployment.yaml``:

```yaml
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: quote
  namespace: backend
spec:
  replicas: 1
  selector:
    matchLabels:
      app: quote
  strategy:
    type: RollingUpdate
  template:
    metadata:
      labels:
        app: quote
    spec:
      containers:
        - name: backend
          image: docker.io/datawire/quote:0.4.1
          ports:
            - name: http
              containerPort: 8080
          resources:
            requests:
              cpu: 100m
              memory: 50Mi
            limits:
              cpu: 200m
              memory: 100Mi
```

The quote service in ``quote_service.yaml``:

```yaml
---
apiVersion: v1
kind: Service
metadata:
  name: quote
  namespace: backend
spec:
  ports:
    - name: http
      port: 80
      targetPort: 8080
  selector:
    app: quote
```

The quote host in ``quote_host.yaml``. You have to change two occurences of ``api.example.com`` to your host. This is also where I decided to use
a ``cluster-issuer`` instead of an ``issuer``:

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: ingress-quote
  namespace: backend
  annotations:
    # cert-manager.io/issuer: letsencrypt-nginx
    cert-manager.io/cluster-issuer: letsencrypt-nginx
spec:
  tls:
    - hosts:
      - api.example.com # CHANGE THIS
      secretName: letsencrypt-nginx-quote
  rules:
    - host: api.example.com # CHANGE THIS
      http:
        paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: quote
                port:
                  number: 80
  ingressClassName: nginx
```

Apply the three files in a ``backend`` namespace:

```shell
kubectl create ns backend
kubectl apply -f quote_deployment.yaml
kubectl apply -f quote_service.yaml
kubectl apply -f quote_host.yaml
```

# Install cert-manager

```shell
helm repo add jetstack https://charts.jetstack.io
helm repo update jetstack

helm install \
  cert-manager jetstack/cert-manager \
  --namespace cert-manager \
  --create-namespace \
  --version v1.14.5 \
  --set installCRDs=true
```

# Install the certificate issuer

We use [Let's Encrypt](https://letsencrypt.org/) with the ``http01`` solver. An alternative is to use a DNS solver like
[acme/dns01/digitalocean/](https://cert-manager.io/docs/configuration/acme/dns01/digitalocean/).

Set your own email in ``clusterissuer-letsencrypt-nginx.yaml``:

```yaml
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata:
  name: letsencrypt-nginx
spec:
  acme:
    server: https://acme-v02.api.letsencrypt.org/directory
    email: example@example.com # CHANGE THIS
    privateKeySecretRef:
      name: letsencrypt-nginx
    solvers:
    - http01:
        ingress:
          class: nginx
```

Apply the cluster issuer:

```shell
kubectl apply -f clusterissuer-letsencrypt-nginx.yaml
```

# Wait for the certificate

Check the status of the certificate. You'll see its ``READY`` value change from ``False`` to ``True``:

```shell-session
$ kubectl get certificates -n backend
NAME                      READY   SECRET                    AGE
letsencrypt-nginx-quote   False   letsencrypt-nginx-quote   70s

$ kubectl get certificates -n backend
NAME                      READY   SECRET                    AGE
letsencrypt-nginx-quote   True    letsencrypt-nginx-quote   96s
```

Check that you can query the endpoint:

```shell-session
$ curl https://api.example.com
{
    "server": "meaty-raspberry-jptbx237",
    "quote": "A principal idea is omnipresent, much like candy.",
    "time": "2024-05-30T01:14:06.094422251Z"
}
```

# Debugging

Check all the details of the certificate request (some IDs redacted here):

```shell-session
$ kubectl describe certificaterequest letsencrypt-nginx-quote-1 -n backend
Name:         letsencrypt-nginx-quote-1
Namespace:    backend
Labels:       <none>
Annotations:  cert-manager.io/certificate-name: letsencrypt-nginx-quote
              cert-manager.io/certificate-revision: 1
              cert-manager.io/private-key-secret-name: letsencrypt-nginx-quote-g7l2r
API Version:  cert-manager.io/v1
Kind:         CertificateRequest
Metadata:
  Creation Timestamp:  2024-05-30T01:12:52Z
  Generation:          1
  Owner References:
    API Version:           cert-manager.io/v1
    Block Owner Deletion:  true
    Controller:            true
    Kind:                  Certificate
    Name:                  letsencrypt-nginx-quote
    UID:                   00000000-0000-0000-0000-000000000000
  Resource Version:        7799
  UID:                     00000000-0000-0000-0000-000000000000
Spec:
  Extra:
    authentication.kubernetes.io/credential-id:
      JTI=00000000-0000-0000-0000-000000000000
    authentication.kubernetes.io/node-name:
      pool-000000000-00000
    authentication.kubernetes.io/node-uid:
      00000000-0000-0000-0000-000000000000
    authentication.kubernetes.io/pod-name:
      cert-manager-0000000000-00000
    authentication.kubernetes.io/pod-uid:
      00000000-0000-0000-0000-000000000000
  Groups:
    system:serviceaccounts
    system:serviceaccounts:cert-manager
    system:authenticated
  Issuer Ref:
    Group:  cert-manager.io
    Kind:   ClusterIssuer
    Name:   letsencrypt-nginx
  Request:  LS0tLS1C<redacted>
  UID:      00000000-0000-0000-0000-000000000000
  Usages:
    digital signature
    key encipherment
  Username:  system:serviceaccount:cert-manager:cert-manager
Status:
  Certificate:  LS0tLS1CR<redacted>
  Conditions:
    Last Transition Time:  2024-05-30T01:12:52Z
    Message:               Certificate request has been approved by cert-manager.io
    Reason:                cert-manager.io
    Status:                True
    Type:                  Approved
    Last Transition Time:  2024-05-30T01:13:33Z
    Message:               Certificate fetched from issuer successfully
    Reason:                Issued
    Status:                True
    Type:                  Ready
Events:
  Type    Reason              Age    From                                                Message
  ----    ------              ----   ----                                                -------
  Normal  WaitingForApproval  3m16s  cert-manager-certificaterequests-issuer-acme        Not signing CertificateRequest until it is Approved
  Normal  WaitingForApproval  3m16s  cert-manager-certificaterequests-issuer-vault       Not signing CertificateRequest until it is Approved
  Normal  WaitingForApproval  3m16s  cert-manager-certificaterequests-issuer-ca          Not signing CertificateRequest until it is Approved
  Normal  WaitingForApproval  3m16s  cert-manager-certificaterequests-issuer-venafi      Not signing CertificateRequest until it is Approved
  Normal  WaitingForApproval  3m16s  cert-manager-certificaterequests-issuer-selfsigned  Not signing CertificateRequest until it is Approved
  Normal  cert-manager.io     3m16s  cert-manager-certificaterequests-approver           Certificate request has been approved by cert-manager.io
  Normal  IssuerNotFound      3m16s  cert-manager-certificaterequests-issuer-acme        Referenced "ClusterIssuer" not found: clusterissuer.cert-manager.io "letsencrypt-nginx" not found
  Normal  IssuerNotFound      3m16s  cert-manager-certificaterequests-issuer-selfsigned  Referenced "ClusterIssuer" not found: clusterissuer.cert-manager.io "letsencrypt-nginx" not found
  Normal  IssuerNotFound      3m16s  cert-manager-certificaterequests-issuer-vault       Referenced "ClusterIssuer" not found: clusterissuer.cert-manager.io "letsencrypt-nginx" not found
  Normal  IssuerNotFound      3m16s  cert-manager-certificaterequests-issuer-ca          Referenced "ClusterIssuer" not found: clusterissuer.cert-manager.io "letsencrypt-nginx" not found
  Normal  IssuerNotFound      3m16s  cert-manager-certificaterequests-issuer-venafi      Referenced "ClusterIssuer" not found: clusterissuer.cert-manager.io "letsencrypt-nginx" not found
  Normal  CertificateIssued   2m35s  cert-manager-certificaterequests-issuer-acme        Certificate fetched from issuer successfully
```

The ``Events`` show the progression from ``WaitingForApproval`` through to ``CertificateIssued``.

To debug further, find the cert-manager pod:

```shell-session
$ kubectl -n cert-manager get pods
NAME                                       READY   STATUS    RESTARTS   AGE
cert-manager-788887dcf6-5scwt              1/1     Running   0          7m25s
cert-manager-cainjector-7db9485cdd-fm8pk   1/1     Running   0          7m25s
cert-manager-webhook-6c79485fc4-8d2kz      1/1     Running   0          7m25s
```

And ask for its logs:

```shell-session
$ kubectl -n cert-manager logs cert-manager-788887dcf6-5scwt
I0530 01:12:01.081341       1 controller.go:288] "configured acme dns01 nameservers" logger="cert-manager.controller.build-context" nameservers=["10.245.0.10:53"]
W0530 01:12:01.081906       1 client_config.go:618] Neither --kubeconfig nor --master was specified.  Using the inClusterConfig.  This might not work.
I0530 01:12:01.086300       1 controller.go:89] "enabled controllers: [certificaterequests-approver certificaterequests-issuer-acme certificaterequests-issuer-ca certificaterequests-issuer-selfsigned certificaterequests-issuer-vault certificaterequests-issuer-venafi certificates-issuing certificates-key-manager certificates-metrics certificates-readiness certificates-request-manager certificates-revision-manager certificates-trigger challenges clusterissuers ingress-shim issuers orders]" logger="cert-manager.controller"
I0530 01:12:01.086651       1 controller.go:435] "serving insecurely as tls certificate data not provided" logger="cert-manager.controller"
I0530 01:12:01.086941       1 controller.go:102] "listening for insecure connections" logger="cert-manager.controller" address="0.0.0.0:9402"
I0530 01:12:01.087931       1 controller.go:182] "starting leader election" logger="cert-manager.controller"
I0530 01:12:01.093936       1 leaderelection.go:250] attempting to acquire leader lease kube-system/cert-manager-controller...
I0530 01:12:01.095387       1 controller.go:129] "starting metrics server" logger="cert-manager.controller" address="[::]:9402"
I0530 01:12:01.095893       1 controller.go:175] "starting healthz server" logger="cert-manager.controller" address="[::]:9403"
I0530 01:12:01.125599       1 leaderelection.go:260] successfully acquired lease kube-system/cert-manager-controller
I0530 01:12:01.170487       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificates-readiness"
I0530 01:12:01.185511       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificates-trigger"
I0530 01:12:01.186389       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificates-request-manager"
I0530 01:12:01.186752       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificates-revision-manager"
I0530 01:12:01.205695       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificaterequests-issuer-venafi"
I0530 01:12:01.206379       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificaterequests-issuer-acme"
I0530 01:12:01.206649       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificaterequests-issuer-selfsigned"
I0530 01:12:01.239168       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="issuers"
I0530 01:12:01.239619       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificaterequests-approver"
I0530 01:12:01.239964       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="challenges"
I0530 01:12:01.250159       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificates-metrics"
I0530 01:12:01.250938       1 controller.go:229] "not starting controller as it's disabled" logger="cert-manager.controller" controller="certificatesigningrequests-issuer-ca"
I0530 01:12:01.251198       1 controller.go:229] "not starting controller as it's disabled" logger="cert-manager.controller" controller="certificatesigningrequests-issuer-vault"
I0530 01:12:01.251395       1 controller.go:229] "not starting controller as it's disabled" logger="cert-manager.controller" controller="certificatesigningrequests-issuer-venafi"
I0530 01:12:01.265868       1 controller.go:229] "not starting controller as it's disabled" logger="cert-manager.controller" controller="certificatesigningrequests-issuer-acme"
I0530 01:12:01.266249       1 controller.go:229] "not starting controller as it's disabled" logger="cert-manager.controller" controller="gateway-shim"
I0530 01:12:01.266473       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificaterequests-issuer-ca"
I0530 01:12:01.278319       1 controller.go:229] "not starting controller as it's disabled" logger="cert-manager.controller" controller="certificatesigningrequests-issuer-selfsigned"
I0530 01:12:01.266546       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificaterequests-issuer-vault"
I0530 01:12:01.286858       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificates-key-manager"
I0530 01:12:01.287193       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="orders"
I0530 01:12:01.266583       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="ingress-shim"
I0530 01:12:01.293933       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="clusterissuers"
I0530 01:12:01.294209       1 controller.go:252] "starting controller" logger="cert-manager.controller" controller="certificates-issuing"
I0530 01:12:01.300288       1 reflector.go:351] Caches populated for *v1.PartialObjectMetadata from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
I0530 01:12:01.302231       1 reflector.go:351] Caches populated for *v1.Certificate from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
I0530 01:12:01.307493       1 reflector.go:351] Caches populated for *v1.Secret from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
I0530 01:12:01.308084       1 reflector.go:351] Caches populated for *v1.PartialObjectMetadata from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
I0530 01:12:01.308520       1 reflector.go:351] Caches populated for *v1.PartialObjectMetadata from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
I0530 01:12:01.308911       1 reflector.go:351] Caches populated for *v1.Ingress from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
I0530 01:12:01.313041       1 reflector.go:351] Caches populated for *v1.CertificateRequest from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
I0530 01:12:01.333157       1 reflector.go:351] Caches populated for *v1.Challenge from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
I0530 01:12:01.340627       1 reflector.go:351] Caches populated for *v1.Order from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
I0530 01:12:01.356731       1 reflector.go:351] Caches populated for *v1.ClusterIssuer from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
I0530 01:12:01.368987       1 reflector.go:351] Caches populated for *v1.Issuer from k8s.io/client-go@v0.29.0/tools/cache/reflector.go:229
E0530 01:12:06.424256       1 controller.go:167] "re-queuing item due to error processing" err="Internal error occurred: failed calling webhook \"webhook.cert-manager.io\": failed to call webhook: Post \"https://cert-manager-webhook.cert-manager.svc:443/validate?timeout=30s\": http: server gave HTTP response to HTTPS client" logger="cert-manager.ingress-shim" key="backend/ingress-quote"
I0530 01:12:11.448092       1 conditions.go:203] Setting lastTransitionTime for Certificate "letsencrypt-nginx-quote" condition "Ready" to 2024-05-30 01:12:11.448076703 +0000 UTC m=+10.459272850
I0530 01:12:11.449259       1 trigger_controller.go:215] "Certificate must be re-issued" logger="cert-manager.certificates-trigger" key="backend/letsencrypt-nginx-quote" reason="DoesNotExist" message="Issuing certificate as Secret does not exist"
I0530 01:12:11.449675       1 conditions.go:203] Setting lastTransitionTime for Certificate "letsencrypt-nginx-quote" condition "Issuing" to 2024-05-30 01:12:11.449664317 +0000 UTC m=+10.460860482
I0530 01:12:11.478583       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificates-trigger" key="backend/letsencrypt-nginx-quote" error="Operation cannot be fulfilled on certificates.cert-manager.io \"letsencrypt-nginx-quote\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:11.479664       1 trigger_controller.go:215] "Certificate must be re-issued" logger="cert-manager.certificates-trigger" key="backend/letsencrypt-nginx-quote" reason="DoesNotExist" message="Issuing certificate as Secret does not exist"
I0530 01:12:11.480161       1 conditions.go:203] Setting lastTransitionTime for Certificate "letsencrypt-nginx-quote" condition "Issuing" to 2024-05-30 01:12:11.480131149 +0000 UTC m=+10.491327332
I0530 01:12:12.212167       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Approved" to 2024-05-30 01:12:12.212154571 +0000 UTC m=+11.223350717
I0530 01:12:12.270372       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:12.270358994 +0000 UTC m=+11.281555136
I0530 01:12:12.273336       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:12.273293642 +0000 UTC m=+11.284489786
I0530 01:12:12.274217       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:12.274179634 +0000 UTC m=+11.285375785
I0530 01:12:12.274963       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:12.27495717 +0000 UTC m=+11.286153308
I0530 01:12:12.276769       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:12.27676107 +0000 UTC m=+11.287957211
I0530 01:12:12.320515       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-selfsigned" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:12.335817       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-acme" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:12.338456       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-venafi" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:12.345643       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-vault" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:33.601515       1 requestmanager_controller.go:323] "CertificateRequest does not match requirements on certificate.spec, deleting CertificateRequest" logger="cert-manager.certificates-request-manager" key="backend/letsencrypt-nginx-quote" related_resource_name="letsencrypt-nginx-quote-1" related_resource_namespace="backend" related_resource_kind="CertificateRequest" related_resource_version="v1" violations=["spec.dnsNames"]
I0530 01:12:33.715374       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Approved" to 2024-05-30 01:12:33.715359334 +0000 UTC m=+32.726555516
I0530 01:12:33.763728       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:33.763666699 +0000 UTC m=+32.774862860
I0530 01:12:33.766416       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:33.76640383 +0000 UTC m=+32.777599974
I0530 01:12:33.767204       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:33.767194431 +0000 UTC m=+32.778390587
I0530 01:12:33.767802       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:33.767792928 +0000 UTC m=+32.778989083
I0530 01:12:33.768591       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:33.768581451 +0000 UTC m=+32.779777609
E0530 01:12:33.773817       1 controller.go:167] "re-queuing item due to error processing" err="certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\" already exists" logger="cert-manager.certificates-request-manager" key="backend/letsencrypt-nginx-quote"
I0530 01:12:33.838634       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-venafi" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:33.854508       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-vault" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:33.861854       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-acme" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:33.865184       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-ca" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:52.066274       1 requestmanager_controller.go:323] "CertificateRequest does not match requirements on certificate.spec, deleting CertificateRequest" logger="cert-manager.certificates-request-manager" key="backend/letsencrypt-nginx-quote" related_resource_name="letsencrypt-nginx-quote-1" related_resource_namespace="backend" related_resource_kind="CertificateRequest" related_resource_version="v1" violations=["spec.dnsNames"]
I0530 01:12:52.196342       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Approved" to 2024-05-30 01:12:52.196326093 +0000 UTC m=+51.207522228
I0530 01:12:52.249154       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:52.249132242 +0000 UTC m=+51.260328382
I0530 01:12:52.249995       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:52.249988226 +0000 UTC m=+51.261184346
I0530 01:12:52.250736       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:52.250729633 +0000 UTC m=+51.261925772
I0530 01:12:52.251579       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:52.25157229 +0000 UTC m=+51.262768429
I0530 01:12:52.252596       1 conditions.go:263] Setting lastTransitionTime for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready" to 2024-05-30 01:12:52.252589737 +0000 UTC m=+51.263785863
I0530 01:12:52.288758       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-venafi" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:52.289250       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-acme" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:52.295417       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-ca" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:12:52.297375       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificaterequests-issuer-selfsigned" key="backend/letsencrypt-nginx-quote-1" error="Operation cannot be fulfilled on certificaterequests.cert-manager.io \"letsencrypt-nginx-quote-1\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:13:08.947228       1 setup.go:113] "generating acme account private key" logger="cert-manager.clusterissuers" resource_name="letsencrypt-nginx" resource_namespace="" resource_kind="ClusterIssuer" resource_version="v1" related_resource_name="letsencrypt-nginx" related_resource_namespace="cert-manager" related_resource_kind="Secret"
I0530 01:13:09.063841       1 setup.go:225] "ACME server URL host and ACME private key registration host differ. Re-checking ACME account registration" logger="cert-manager.clusterissuers" resource_name="letsencrypt-nginx" resource_namespace="" resource_kind="ClusterIssuer" resource_version="v1" related_resource_name="letsencrypt-nginx" related_resource_namespace="cert-manager" related_resource_kind="Secret"
I0530 01:13:09.854010       1 setup.go:315] "verified existing registration with ACME server" logger="cert-manager.clusterissuers" resource_name="letsencrypt-nginx" resource_namespace="" resource_kind="ClusterIssuer" resource_version="v1" related_resource_name="letsencrypt-nginx" related_resource_namespace="cert-manager" related_resource_kind="Secret"
I0530 01:13:09.854466       1 conditions.go:96] Setting lastTransitionTime for Issuer "letsencrypt-nginx" condition "Ready" to 2024-05-30 01:13:09.85444073 +0000 UTC m=+68.865636892
I0530 01:13:09.892503       1 setup.go:208] "skipping re-verifying ACME account as cached registration details look sufficient" logger="cert-manager.clusterissuers" resource_name="letsencrypt-nginx" resource_namespace="" resource_kind="ClusterIssuer" resource_version="v1" related_resource_name="letsencrypt-nginx" related_resource_namespace="cert-manager" related_resource_kind="Secret"
W0530 01:13:11.526535       1 warnings.go:70] metadata.finalizers: "finalizer.acme.cert-manager.io": prefer a domain-qualified finalizer name to avoid accidental conflicts with other finalizer writers
I0530 01:13:11.693209       1 pod.go:71] "creating HTTP01 challenge solver pod" logger="cert-manager.challenges.http01.ensurePod" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01"
W0530 01:13:12.152112       1 warnings.go:70] annotation "kubernetes.io/ingress.class" is deprecated, please use 'spec.ingressClassName' instead
I0530 01:13:12.153297       1 pod.go:59] "found one existing HTTP01 solver pod" logger="cert-manager.challenges.http01.selfCheck.http01.ensurePod" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01" related_resource_name="cm-acme-http-solver-pvcr2" related_resource_namespace="backend" related_resource_kind="" related_resource_version=""
I0530 01:13:12.153436       1 service.go:45] "found one existing HTTP01 solver Service for challenge resource" logger="cert-manager.challenges.http01.selfCheck.http01.ensureService" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01" related_resource_name="cm-acme-http-solver-znf8m" related_resource_namespace="backend" related_resource_kind="" related_resource_version=""
I0530 01:13:12.154926       1 ingress.go:99] "found one existing HTTP01 solver ingress" logger="cert-manager.challenges.http01.selfCheck.http01.ensureIngress" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01" related_resource_name="cm-acme-http-solver-zr6cb" related_resource_namespace="backend" related_resource_kind="" related_resource_version=""
E0530 01:13:12.302627       1 sync.go:190] "propagation check failed" err="wrong status code '404', expected '200'" logger="cert-manager.challenges" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01"
I0530 01:13:12.326831       1 pod.go:59] "found one existing HTTP01 solver pod" logger="cert-manager.challenges.http01.selfCheck.http01.ensurePod" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01" related_resource_name="cm-acme-http-solver-pvcr2" related_resource_namespace="backend" related_resource_kind="" related_resource_version=""
I0530 01:13:12.327277       1 service.go:45] "found one existing HTTP01 solver Service for challenge resource" logger="cert-manager.challenges.http01.selfCheck.http01.ensureService" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01" related_resource_name="cm-acme-http-solver-znf8m" related_resource_namespace="backend" related_resource_kind="" related_resource_version=""
I0530 01:13:12.327480       1 ingress.go:99] "found one existing HTTP01 solver ingress" logger="cert-manager.challenges.http01.selfCheck.http01.ensureIngress" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01" related_resource_name="cm-acme-http-solver-zr6cb" related_resource_namespace="backend" related_resource_kind="" related_resource_version=""
E0530 01:13:12.370401       1 sync.go:190] "propagation check failed" err="wrong status code '404', expected '200'" logger="cert-manager.challenges" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01"
I0530 01:13:14.079343       1 setup.go:208] "skipping re-verifying ACME account as cached registration details look sufficient" logger="cert-manager.clusterissuers" resource_name="letsencrypt-nginx" resource_namespace="" resource_kind="ClusterIssuer" resource_version="v1" related_resource_name="letsencrypt-nginx" related_resource_namespace="cert-manager" related_resource_kind="Secret"
I0530 01:13:22.304527       1 pod.go:59] "found one existing HTTP01 solver pod" logger="cert-manager.challenges.http01.selfCheck.http01.ensurePod" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01" related_resource_name="cm-acme-http-solver-pvcr2" related_resource_namespace="backend" related_resource_kind="" related_resource_version=""
I0530 01:13:22.304891       1 service.go:45] "found one existing HTTP01 solver Service for challenge resource" logger="cert-manager.challenges.http01.selfCheck.http01.ensureService" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01" related_resource_name="cm-acme-http-solver-znf8m" related_resource_namespace="backend" related_resource_kind="" related_resource_version=""
I0530 01:13:22.305114       1 ingress.go:99] "found one existing HTTP01 solver ingress" logger="cert-manager.challenges.http01.selfCheck.http01.ensureIngress" resource_name="letsencrypt-nginx-quote-1-3066565044-3627852778" resource_namespace="backend" resource_kind="Challenge" resource_version="v1" dnsName="api.example.com" type="HTTP-01" related_resource_name="cm-acme-http-solver-zr6cb" related_resource_namespace="backend" related_resource_kind="" related_resource_version=""
E0530 01:13:32.788230       1 controller.go:98] ingress 'backend/cm-acme-http-solver-zr6cb' in work queue no longer exists
I0530 01:13:33.439828       1 acme.go:233] "certificate issued" logger="cert-manager.certificaterequests-issuer-acme.sign" resource_name="letsencrypt-nginx-quote-1" resource_namespace="backend" resource_kind="CertificateRequest" resource_version="v1" related_resource_name="letsencrypt-nginx-quote-1-3066565044" related_resource_namespace="backend" related_resource_kind="Order" related_resource_version="v1"
I0530 01:13:33.440197       1 conditions.go:252] Found status change for CertificateRequest "letsencrypt-nginx-quote-1" condition "Ready": "False" -> "True"; setting lastTransitionTime to 2024-05-30 01:13:33.440165326 +0000 UTC m=+92.451361465
I0530 01:13:33.493742       1 conditions.go:192] Found status change for Certificate "letsencrypt-nginx-quote" condition "Ready": "False" -> "True"; setting lastTransitionTime to 2024-05-30 01:13:33.493732423 +0000 UTC m=+92.504928566
I0530 01:13:33.521808       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificates-readiness" key="backend/letsencrypt-nginx-quote" error="Operation cannot be fulfilled on certificates.cert-manager.io \"letsencrypt-nginx-quote\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:13:33.525363       1 conditions.go:192] Found status change for Certificate "letsencrypt-nginx-quote" condition "Ready": "False" -> "True"; setting lastTransitionTime to 2024-05-30 01:13:33.525352501 +0000 UTC m=+92.536548642
I0530 01:13:33.547107       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.certificates-readiness" key="backend/letsencrypt-nginx-quote" error="Operation cannot be fulfilled on certificates.cert-manager.io \"letsencrypt-nginx-quote\": the object has been modified; please apply your changes to the latest version and try again"
I0530 01:13:33.548898       1 conditions.go:192] Found status change for Certificate "letsencrypt-nginx-quote" condition "Ready": "False" -> "True"; setting lastTransitionTime to 2024-05-30 01:13:33.548889361 +0000 UTC m=+92.560085500
E0530 01:13:33.868842       1 sync.go:73] "failed to update status" logger="cert-manager.orders" resource_name="letsencrypt-nginx-quote-1-3066565044" resource_namespace="backend" resource_kind="Order" resource_version="v1"
I0530 01:13:33.869155       1 controller.go:162] "re-queuing item due to optimistic locking on resource" logger="cert-manager.orders" key="backend/letsencrypt-nginx-quote-1-3066565044" error="Operation cannot be fulfilled on orders.acme.cert-manager.io \"letsencrypt-nginx-quote-1-3066565044\": the object has been modified; please apply your changes to the latest version and try again"
E0530 01:13:33.904307       1 controller.go:208] "challenge in work queue no longer exists" err="challenge.acme.cert-manager.io \"letsencrypt-nginx-quote-1-3066565044-3627852778\" not found" logger="cert-manager.challenges"
```

The messages ``the object has been modified; please apply your changes to the latest version and try again`` are a bit misleading; see
[cert-manager/issues/3501](https://github.com/cert-manager/cert-manager/issues/3501), in
particular [this comment](https://github.com/cert-manager/cert-manager/issues/3501#issuecomment-886841224).

Finally, check the details of the certificate:

```shell-session
$ kubectl describe certificate letsencrypt-nginx-quote -n backend
Name:         letsencrypt-nginx-quote
Namespace:    backend
Labels:       <none>
Annotations:  <none>
API Version:  cert-manager.io/v1
Kind:         Certificate
Metadata:
  Creation Timestamp:  2024-05-30T01:12:11Z
  Generation:          3
  Owner References:
    API Version:           networking.k8s.io/v1
    Block Owner Deletion:  true
    Controller:            true
    Kind:                  Ingress
    Name:                  ingress-quote
    UID:                   00000000-0000-0000-0000-000000000000
  Resource Version:        7805
  UID:                     00000000-0000-0000-0000-000000000000
Spec:
  Dns Names:
    api.example.com
  Issuer Ref:
    Group:      cert-manager.io
    Kind:       ClusterIssuer
    Name:       letsencrypt-nginx
  Secret Name:  letsencrypt-nginx-quote
  Usages:
    digital signature
    key encipherment
Status:
  Conditions:
    Last Transition Time:  2024-05-30T01:13:33Z
    Message:               Certificate is up to date and has not expired
    Observed Generation:   3
    Reason:                Ready
    Status:                True
    Type:                  Ready
  Not After:               2024-08-28T00:13:31Z
  Not Before:              2024-05-30T00:13:32Z
  Renewal Time:            2024-07-29T00:13:31Z
  Revision:                1
Events:
  Type     Reason         Age                From                                       Message
  ----     ------         ----               ----                                       -------
  Normal   Issuing        16m                cert-manager-certificates-trigger          Issuing certificate as Secret does not exist
  Normal   Generated      16m                cert-manager-certificates-key-manager      Stored new private key in temporary Secret resource "letsencrypt-nginx-quote-g7l2r"
  Warning  RequestFailed  16m                cert-manager-certificates-request-manager  Failed to create CertificateRequest: certificaterequests.cert-manager.io "letsencrypt-nginx-quote-1" already exists
  Normal   Requested      16m (x3 over 16m)  cert-manager-certificates-request-manager  Created new CertificateRequest resource "letsencrypt-nginx-quote-1"
  Normal   Issuing        15m                cert-manager-certificates-issuing          The certificate has been successfully issued
```

This shows that the certificate was successfully issued.

# Further reading (watching)

{{< youtube MpovOI5eK58 >}}





