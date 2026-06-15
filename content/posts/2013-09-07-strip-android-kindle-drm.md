---
author: Carlo Hamalainen

date: "2013-09-07T00:00:00Z"
format: image
title: Strip Android Kindle DRM
url: /2013/09/07/strip-android-kindle-drm/
---
I bought an ebook from Amazon using their Android Kindle app. Unfortunately the Android app is crippleware in that you can't export your highlights and notes. So I looked into how to strip the DRM so that I could read the ebook on my Linux (Debian) laptop using a non-DRM-encumbered application. Also, [past behaviour](http://www.nytimes.com/2009/07/18/technology/companies/18amazon.html?_r=0) of Amazon doesn't inspire confidence. 

I tried using [Apprentice Alf's tools](http://apprenticealf.wordpress.com/) with Calibre on Debian but the decryption didn't work. I'm not 100% sure but it seemed to be missing the PID of the ebook (the PRC file on my Android device). Some people have written [patches for the Android Kindle app](https://github.com/psyrendust/dedrm-ebook-tools/tree/master/Other_Tools/Kindle_for_Android_Patches) so that you can view the ebook's PID, but they are not up to date. And frankly, patching an apk is a fairly involved process. 

The work-around is to run Calibre and Apprentice Alf's tools on Windows. Here are the details: 

1. Install [Calibre on Windows](http://calibre-ebook.com/download_windows). 

2. Install the [Kindle PC application](http://www.amazon.com/gp/kindle/pc/download) from Amazon. 

3. Install [Apprentice Alf's tools](http://apprenticealf.wordpress.com/) in Calibre. You want to point Calibre at the  
file ``DeDRM_calibre_plugin/DeDRM_plugin.zip`` which is inside ``tools_v6.0.8.zip`` (don't unzip ``DeDRM_plugin.zip``!).  
Local mirror: [alfs_tools](/stuff/alfs_tools).

4. Buy an ebook using the Android app (or any linked device). 

5. In Kindle PC, sync the book. 

6. In Calibre, import the book. You'll find it in My DocumentsMy Kindle Content. When you import the book in Calibre, Alf's plugin will automatically strip the DRM. 

7. The file in My DocumentsCalibre can now be copied to another device, for example imported into Calibre on a Linux/Debian system. 

Here are some screenshots for steps 3, 6, and 7: <http://www.sanspantalones.com/2013/05/30/how-to-remove-drm-from-your-kindle-books/>.

Overall, this is a huge pain. At least with my music purchases I can support artists on a site that does not use DRM: [bandcamp.com](http://bandcamp.com/). 

