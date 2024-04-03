---
author: Carlo Hamalainen

date: "2013-09-07T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2013/09/07/strip-android-kindle-drm/
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

**Archived Comments**

Date: 2013-12-11 21:51:36.885022 UTC

Author: J

Man spent so much time trying to figure this out, but this made it so easy

Date: 2013-12-18 11:20:08.632469 UTC

Author: Tav

Sadly this doesn't work for many eMags, the Kindle PC App won't download them

Date: 2015-02-07 14:13:05.395909 UTC

Author: Brian J Hoskins

Ebook providers try to lock you in to their services; that's their business model, and it's how they make their money. But it's extremely frustrating for the end-user. You end up financially committed to a provider because you've purchased many books from them. It happened to me with Google Books. Someone else could have come along with a much better Ebook provider, but I'd still feel like I was shackled to Google Books because I already 'own' significant content there.

The problem is then exascerbated when you find that a book you wanted isn't available from your regular service provider, but it's available at one of the others. So then you become locked in to \*two\* Ebook service providers! And you have to download their apps in order to read the books, which means you can't always access the content on portable devices when you need it, and etc etc etc. The whole situation is a real pain in the back side. 

I recently decided enough was enough. I now host my Ebooks myself! I still purchase them legitimately from Ebook service providers, but I strip the DRM using the methods highlighted in this article, then I manage the book in Calibre and host my Calibre library on my own server, thereby freeing me from the frustration of online Ebook service providers. 

<http://brianhoskins.uk/hosting-a-calibre-library-online/> 

Date: 2015-05-18 19:56:46.732147 UTC

Author: bráulio

thanks, it worked! great free software (as in freedom!)
