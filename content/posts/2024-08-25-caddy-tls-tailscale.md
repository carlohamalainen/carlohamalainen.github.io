---
date: 2024-08-25T08:16:02+08:00
title: Caddy TLS Tailscale
author: "Carlo Hamalainen"
url: /2024/08/25/2024-08-25-caddy-tls-tailscale
---

Setting up TLS with Caddy on a Digital Ocean droplet is so easy with the [Caddy DNS plugin](https://github.com/caddy-dns/digitalocean).

I set up a new droplet with [Tailscale](https://tailscale.com/) and found the challenge kept failing with an error
``could not determine zone for domain``:

```
Aug 08 11:04:37 ubuntu caddy[10197]: {"level":"error","ts":1723115077.6617424,"logger":"tls","msg":"job failed",
"error":"foo.example.com: obtaining certificate: [foo.example.com] Obtain: [foo.example.com] solving
challenges: presenting for challenge: could not determine zone for domain \"_acme-challenge.foo.example.com\":
unexpected response code 'SERVFAIL' for _acme-challenge.foo.example.com.
(order=https://acme-v02.api.letsencrypt.org/acme/order/0000000000/000000000000)
(ca=https://acme-v02.api.letsencrypt.org/directory)"}
```

The fix seems to be to add custom ``resolvers``. Using Google's 8888/8844 DNS works:

```
foo.example.com {
    route /v1/* {
        reverse_proxy 0.0.0.0:3000
    }

    tls {
        dns digitalocean dop_v1_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

        resolvers 8.8.8.8 8.8.4.4
    }
}
```