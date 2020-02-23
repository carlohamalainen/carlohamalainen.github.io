---
id: 692
title: SweetPea3 disk image
date: 2010-04-03T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2010/04/03/sweetpea3-disk-image/
permalink: /2010/04/03/sweetpea3-disk-image/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
A while ago I bought my son a [SweetPea3](http://www.sweetpeatoyco.com/) MP3 player. It&#8217;s great but occasionally the files get garbled and it crashes (a firmware update is meant to fix this issue &#8211; see the SweetPea3 site).

Before I loaded any songs onto the SweetPea3 I saved an image of the disk. To recover, assuming that /dev/sdb is your SweetPea3, do this as root:

<pre># bzcat sweetpea3_image_SP3-101.bz2 &gt; /dev/sdb
</pre>



The image file is available [here](http://carlo-hamalainen.net/sweetpea3_image/) and beware that **the image is for the SP3-101 version of the SweetPea3**. Also note that the image is of /dev/sdb, not the first partition /dev/sdb1.