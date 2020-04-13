---
id: 777
title: Backup gmail with mpop
date: 2009-12-13T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2009/12/13/backup-gmail-with-mpop/
permalink: /2009/12/13/backup-gmail-with-mpop/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
To backup email from a gmail account, first install mpop:

    sudo apt-get install mpop

Edit your ``~/.mpoprc`` file:

    # defaults
    defaults
    tls on

    # gmail
    account gmail
    host pop.gmail.com
    user myaccount
    password myaccountpassword
    keep on
    only_new off
    tls_starttls off
    delivery mbox ~/Backup/myaccount@gmail.mbox

    # Set a default account
    account default : gmail

To grab some emails:

    mpop --tls-certcheck=off

Original info from [this post](http://vafer.org/blog/20070103073735).

**Archived Comments**

Date: 2014-02-05 20:06:07.883063 UTC

Author: marko

Much WIN! Thanks!
