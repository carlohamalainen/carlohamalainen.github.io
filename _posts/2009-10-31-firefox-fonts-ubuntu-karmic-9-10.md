---
id: 764
title: Firefox fonts Ubuntu Karmic 9.10
date: 2009-10-31T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2009/10/31/firefox-fonts-ubuntu-karmic-9-10/
permalink: /2009/10/31/firefox-fonts-ubuntu-karmic-9-10/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
To fix the strangely large Firefox fonts in Ubuntu Karmic:

    sudo rm /etc/fonts/conf.d/10-hinting-slight.conf
    sudo rm /etc/fonts/conf.d/10-no-sub-pixel.conf
    sudo ln -s /etc/fonts/conf.available/10-hinting-medium.conf /etc/fonts/conf.d/.
    sudo ln -s /etc/fonts/conf.available/10-sub-pixel-rgb.conf /etc/fonts/conf.d/.
    sudo dpkg-reconfigure fontconfig

Thanks to [this post](http://ubuntuforums.org/showpost.php?p=6986051&postcount=7) on the Ubuntu forums.

**Archived Comments**

Date: 2009-11-18 10:03:02 UTC

Author: Edmund

Thanks Carlo! That worked a treat.  
I didn't do the command  
sudo rm /etc/fonts/conf.d/10-no-sub-pixel.conf  
because that file doesn't exist on my system.  
But the problem was sorted using the rest of the commands.

Date: 2009-12-17 14:15:11 UTC

Author: Pierrick Le Gall

thank you carlo

This page came 3rd on a Google search with ubuntu 9.10 firefox fonts and it was the right one.

It worked nice!

Date: 2010-01-16 12:45:18 UTC

Author: Tom

Thanks!
