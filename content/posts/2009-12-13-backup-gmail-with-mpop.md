---
author: Carlo Hamalainen
date: "2009-12-13T00:00:00Z"
format: image
title: Backup gmail with mpop
url: /2009/12/13/backup-gmail-with-mpop/
---

Update 2024-03-09: none of this would work anymore, one uses application-specific passwords to avoid storing the main account password in plain text in ``.mpoprc``!

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
