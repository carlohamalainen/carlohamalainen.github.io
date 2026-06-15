---
author: Carlo Hamalainen

date: "2013-01-24T00:00:00Z"
format: image
title: Python SSL socket echo test with self-signed certificate
url: /2013/01/24/python-ssl-socket-echo-test-with-self-signed-certificate/
---
For testing purposes it is convenient to use a self-signed certificate. Follow [these instructions](https://devcenter.heroku.com/articles/ssl-certificate-self). You will be prompted for a password a few times: 

```
openssl genrsa -des3 -out server.orig.key 2048
openssl rsa -in server.orig.key -out server.key
openssl req -new -key server.key -out server.csr
openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt
```

Here is  **client.py**, slightly modified from the [Python 2.7.3 docs](http://docs.python.org/2/library/ssl.html): 

```python
import socket, ssl, pprint

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

# Require a certificate from the server. We used a self-signed certificate
# so here ca_certs must be the server certificate itself.
ssl_sock = ssl.wrap_socket(s,
                           ca_certs="server.crt",
                           cert_reqs=ssl.CERT_REQUIRED)

ssl_sock.connect(('localhost', 10023))

print repr(ssl_sock.getpeername())
print ssl_sock.cipher()
print pprint.pformat(ssl_sock.getpeercert())

ssl_sock.write("boo!")

if False: # from the Python 2.7.3 docs
    # Set a simple HTTP request -- use httplib in actual code.
    ssl_sock.write("""GET / HTTP/1.0r
    Host: www.verisign.comnn""")

    # Read a chunk of data.  Will not necessarily
    # read all the data returned by the server.
    data = ssl_sock.read()

    # note that closing the SSLSocket will also close the underlying socket
    ssl_sock.close()
```

And here is  **server.py**: 

```python
import socket, ssl

bindsocket = socket.socket()
bindsocket.bind(('', 10023))
bindsocket.listen(5)

def do_something(connstream, data):
    print "do_something:", data
    return False

def deal_with_client(connstream):
    data = connstream.read()
    while data:
        if not do_something(connstream, data):
            break
        data = connstream.read()

while True:
    newsocket, fromaddr = bindsocket.accept()
    connstream = ssl.wrap_socket(newsocket,
                                 server_side=True,
                                 certfile="server.crt",
                                 keyfile="server.key")
    try:
        deal_with_client(connstream)
    finally:
        connstream.shutdown(socket.SHUT_RDWR)
        connstream.close()
```

Note: if you try to use the standard system ca certificates, e.g. on Debian: 

```python
ssl_sock = ssl.wrap_socket(s,
                           ca_certs="/etc/ssl/certs/ca-certificates.crt",
                           cert_reqs=ssl.CERT_REQUIRED)
```

then server.py explodes with: 

```
Traceback (most recent call last):
  File "server.py", line 24, in 
    ssl_version=ssl.PROTOCOL_TLSv1)
  File "/usr/lib/python2.6/ssl.py", line 338, in wrap_socket
    suppress_ragged_eofs=suppress_ragged_eofs)
  File "/usr/lib/python2.6/ssl.py", line 120, in __init__
    self.do_handshake()
  File "/usr/lib/python2.6/ssl.py", line 279, in do_handshake
    self._sslobj.do_handshake()
ssl.SSLError: [Errno 1] _ssl.c:490: error:1408F10B:SSL routines:SSL3_GET_RECORD:wrong version number
```

If you specify the SSL version, e.g. 

```python
connstream = ssl.wrap_socket(newsocket,
                                 server_side=True,
                                 certfile="server.crt",
                                 keyfile="server.key",
                                 ssl_version=ssl.PROTOCOL_TLSv1)
```

then you can run into other problems, e.g.

```
Traceback (most recent call last):
  File "server.py", line 27, in 
    ssl_version=ssl.PROTOCOL_TLSv1)
  File "/usr/lib64/python2.6/ssl.py", line 338, in wrap_socket
    suppress_ragged_eofs=suppress_ragged_eofs)
  File "/usr/lib64/python2.6/ssl.py", line 120, in __init__
    self.do_handshake()
  File "/usr/lib64/python2.6/ssl.py", line 279, in do_handshake
    self._sslobj.do_handshake()
ssl.SSLError: [Errno 1] _ssl.c:490: error:1408F10B:SSL routines:SSL3_GET_RECORD:wrong version number
```

