---
id: 831
title: Create a Joomla password hash/salt database entry in Python
date: 2013-05-06T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/05/06/create-a-joomla-password-hashsalt-database-entry-in-python/
permalink: /2013/05/06/create-a-joomla-password-hashsalt-database-entry-in-python/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Handy for manually resetting a user's password. Tested on Joomla 2.5.

{% highlight python %}
import getpass
import hashlib
import random
import string

password = getpass.getpass('password (not echoed): ')
salt = ''.join(random.choice(string.ascii_lowercase + string.ascii_uppercase + string.digits) for x in range(32))
password_hash = hashlib.md5(password + salt).hexdigest()

print "update CHANGEME_users set password='" + password_hash + ':' + salt + "' where id = ...;"
{% endhighlight %}
