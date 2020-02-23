---
id: 768
title: ggplot2 from Clojure
date: 2011-08-04T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2011/08/04/ggplot2-from-clojure/
permalink: /2011/08/04/ggplot2-from-clojure/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Just a quick note on how to call ggplot2 from Clojure. Install the [rincanter](https://github.com/jolby/rincanter) package. A tip: if you don&#8217;t know what R_HOME is for your system, try this at the R prompt:

<pre>&gt; R.home(component="home")
[1] "/usr/lib64/R"
&gt;
</pre>

So I did export R_HOME=/usr/lib64/R and then rincanter was happy.

When calling qplot, do not use r-eval because this tries to convert the entire plot into a Clojure object, resulting in unmanageable output. Use r-eval-raw instead.

<https://gist.github.com/1124157.js>

I worked out the right commands by copying one of the answers to this [stackoverflow question](http://stackoverflow.com/questions/3515269/ggplot2-hell-with-rpy2-2-0-7-python-2-6-r-2-11-windows-7).