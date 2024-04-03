---
author: Carlo Hamalainen

date: "2014-08-08T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2014/08/08/jwt-verification-using-hs-jose-and-hs-jwt/
title: JWT verification using hs-jose and hs-jwt
url: /2014/08/08/jwt-verification-using-hs-jose-and-hs-jwt/
---
Following up from a [previous blog post](/2014/08/03/haskell-yesod-aaf-rapid-connect-demo), here is how to verify a JWT blob in the context of a Yesod site, using Fraser Tweedale's [hs-jwt](https://github.com/frasertweedale/hs-jwt) package: 

<http://gist-it.appspot.com/github/carlohamalainen/rapid-connect-yesod-demo/blob/jose-and-hs-jwt/Handler/AuthJwt.hs?slice=84:139> 

Here's the branch of my rapid connect demo that uses hs-jwt: <https://github.com/carlohamalainen/rapid-connect-yesod-demo/tree/jose-and-hs-jwt>.
