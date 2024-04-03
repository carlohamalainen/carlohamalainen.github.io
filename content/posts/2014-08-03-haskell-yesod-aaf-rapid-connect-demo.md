---
author: Carlo Hamalainen

date: "2014-08-03T00:00:00Z"
format: image
title: Haskell Yesod AAF Rapid Connect demo
url: /2014/08/03/haskell-yesod-aaf-rapid-connect-demo/
---
Federated identity authorisation is all the rage in the academic environment at the moment, and with good reason. Maintaining user accounts is a pain for devops staff, and end-users don't need yet another username/password to forget. 

In the past, hooking into the Australian Access Federation has required the configuration of a local Shibboleth Service Provider, which is not always an easy task (see for example [mytardis-app-auth-aaf](https://github.com/steveandroulakis/mytardis-app-auth-aaf)). Recently the AAF launched their [Rapid Connect](https://rapid.aaf.edu.au/) service. Instead of Shibboleth and SAML and other scary things, your application merely has to accept a HTTP POST on a pre-defined URL, which the Rapid Connect service passes a [JSON Web Token (JWT)](http://self-issued.info/docs/draft-ietf-oauth-json-web-token.html) which you can verify using the pre-defined secret and an available JWT library. 

I knocked up [an example using a plain Django site](https://github.com/NIF-au/django-rapid-connect-demo) which conveniently hooks into the Django auth module. All the work happens in [views.py](https://github.com/NIF-au/django-rapid-connect-demo/blob/master/mysite/rc/views.py), in particular in the auth function. 

Naturally I wanted to see how to do the same thing in Haskell. So here's a working Yesod project that performs authorisation via Rapid Connect: <https://github.com/carlohamalainen/rapid-connect-yesod-demo>. The key file is [Handler/AuthJwt.hs](https://github.com/carlohamalainen/rapid-connect-yesod-demo/blob/master/Handler/AuthJwt.hs).

<!-- In particular this function which accepts the HTTP POST request:

<http://gist-it.appspot.com/github/carlohamalainen/rapid-connect-yesod-demo/blob/f4e138ec061dfafff8b9870e138bf371c4e2afda/Handler/AuthJwt.hs?slice=54:91> 

-->

I used the [jwt](https://hackage.haskell.org/package/jwt) package to decode and verify the JWT. Later I plan to try [hs-jwt](https://github.com/frasertweedale/hs-jwt) as well. 

I think that the applicative style works well in this setting. I used to find the syntax jarring but now it is fine. The equivalent code in Python achieves a similar goal by wrapping the entire thing in a try/except block. So in some sense the Haskell code gives a finer control of the computed values (since Maybe has a Functor instance).
