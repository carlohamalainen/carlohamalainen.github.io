---
id: 818
title: Python SSL socket echo test with self-signed certificate
date: 2013-01-24T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/01/24/python-ssl-socket-echo-test-with-self-signed-certificate/
permalink: /2013/01/24/python-ssl-socket-echo-test-with-self-signed-certificate/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
For testing purposes it is convenient to use a self-signed certificate. Follow [these instructions](https://devcenter.heroku.com/articles/ssl-certificate-self). You will be prompted for a password a few times: 

<pre>openssl genrsa -des3 -out server.orig.key 2048
openssl rsa -in server.orig.key -out server.key
openssl req -new -key server.key -out server.csr
openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt
</pre>

Here is  **client.py**, slightly modified from the [Python 2.7.3 docs](http://docs.python.org/2/library/ssl.html): 

<pre>import socket, ssl, pprint

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
</pre>

And here is  **server.py**: 

<pre>import socket, ssl

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
</pre>

Note: if you try to use the standard system ca certificates, e.g. on Debian: 

<pre>ssl_sock = ssl.wrap_socket(s,
                           ca_certs="/etc/ssl/certs/ca-certificates.crt",
                           cert_reqs=ssl.CERT_REQUIRED)
</pre>

then server.py explodes with: 

<pre>Traceback (most recent call last):
  File "server.py", line 24, in 
    ssl_version=ssl.PROTOCOL_TLSv1)
  File "/usr/lib/python2.6/ssl.py", line 338, in wrap_socket
    suppress_ragged_eofs=suppress_ragged_eofs)
  File "/usr/lib/python2.6/ssl.py", line 120, in __init__
    self.do_handshake()
  File "/usr/lib/python2.6/ssl.py", line 279, in do_handshake
    self._sslobj.do_handshake()
ssl.SSLError: [Errno 1] _ssl.c:490: error:1408F10B:SSL routines:SSL3_GET_RECORD:wrong version number
</pre>

If you specify the SSL version, e.g. 

<pre>connstream = ssl.wrap_socket(newsocket,
                                 server_side=True,
                                 certfile="server.crt",
                                 keyfile="server.key",
                                 ssl_version=ssl.PROTOCOL_TLSv1)
</pre>

then you can run into other problems, e.g.

<pre>Traceback (most recent call last):
  File "server.py", line 27, in 
    ssl_version=ssl.PROTOCOL_TLSv1)
  File "/usr/lib64/python2.6/ssl.py", line 338, in wrap_socket
    suppress_ragged_eofs=suppress_ragged_eofs)
  File "/usr/lib64/python2.6/ssl.py", line 120, in __init__
    self.do_handshake()
  File "/usr/lib64/python2.6/ssl.py", line 279, in do_handshake
    self._sslobj.do_handshake()
ssl.SSLError: [Errno 1] _ssl.c:490: error:1408F10B:SSL routines:SSL3_GET_RECORD:wrong version number
</pre>

**Archived Comments**

Date: 2014-08-13 19:11:58.59444 UTC

Author: David

Thanks very much. This was extremely helpful.

I would like to offer one odd caveat here. When I create my certificate and key files on Linux, I am unable to connect to the server from a client running on OS X.

Details:

I tested on 3 computers in July and August, 2014:

Windows 7, Python 2.7.6. OpenSSL 1.0.1i  
OS X 10.7.5, Python 2.7.8, OpenSSL 0.9.8y  
Ubuntu 14.04, Python 2.7.6, OpenSSL 1.0.1f

I created a set of certificate and key files on each of the computers. I then ran the client on Windows and OS X and server on all three of the computers using each of the 3 sets of certificate files.

If the certificate files were created on Linux, I could not connect the OS X client to the server, regardless of the server platform. If the certificate files were created on Windows or OS X, all combinations of client and server worked. The Windows client worked against all 3 servers with all three certificate files. The OS X client worked on all three servers if the OS X or Windows certificates were used. But the OS X client failed against all three servers when those servers used the Linux certificate files.

I can offer no explanation of why.

Date: 2015-09-14 07:22:50.526278 UTC

Author: Limey

In order to get this to work with python V3 I had to change:

ssl_sock.write("boo!")

to:

ssl_sock.write("boo!".encode())

No idea if this is the best solution, but it made it work. =)

Date: 2015-11-06 06:06:35.515219 UTC

Author: Ketan Kothari

Thank's for describing with example. Good to use and easy to understand.