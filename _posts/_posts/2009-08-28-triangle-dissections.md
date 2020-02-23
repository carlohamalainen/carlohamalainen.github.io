---
id: 736
title: Triangle dissections
date: 2009-08-28T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2009/08/28/triangle-dissections/
permalink: /2009/08/28/triangle-dissections/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Some of [my research](http://arxiv.org/abs/0907.1789) is on dissections of triangles into equilateral triangles. Here&#8217;s an example:

<img src="https://i2.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissection.png?w=1100&#038;ssl=1" data-recalc-dims="1" /> 

So the outer equilateral triangle is cut up into smaller equilateral triangles, and no triangles overlap except along a common edge or point.

One of the earliest references on triangle dissections is this paper: [The Dissection of Equilateral Triangles into Equilateral Triangles, W.T.Tutte, Proceedings of the Cambridge Philosophical Society, Vol. 44, pages 464-82, 1948](http://carlo-hamalainen.net/stuff/Tutte%20-%20The%20dissection%20of%20equilateral%20triangles%20into%20equilateral%20triangles%20(1948).pdf). There, Tutte showed a connection between equilateral triangle dissections and electrical networks. See also <http://www.squaring.net/tri/twt.html> and the paper [The dissection of rectangles into squares &#8211; R. L. Brooks, C. A. B. Smith, A. H. Stone and W. T. Tutte; 312-340](http://carlo-hamalainen.net/stuff/Brooks,%20Smith,%20Stone,%20Tutte%20-%20The%20dissection%20of%20rectangles%20into%20squares%20(1940).pdf).

A _perfect dissection_ has no two triangles of the same size in the same orientation (up or down). Tutte conjectured that the smallest perfect dissection has size 15, and some recent [enumeration work](http://bitbucket.org/carlohamalainen/dissections/) shows this to be the case (a paper will come out soon with these results). Here are the two perfect dissections of size 15, and perfect dissections of size 16 and 17:

<img src="https://i1.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/perfect_dissection_size15_595_r5_c3.png?w=1100&#038;ssl=1" data-recalc-dims="1" /><img src="https://i0.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/perfect_dissection_size15_595_r5_c4.png?w=1100&#038;ssl=1" data-recalc-dims="1" /> 

<img src="https://i1.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/perfect_dissection_size16_3073_r2_c3.png?w=1100&#038;ssl=1" data-recalc-dims="1" /><img src="https://i2.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/perfect_dissection_size16_3073_r2_c5.png?w=1100&#038;ssl=1" data-recalc-dims="1" /> 

<img src="https://i2.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/perfect_dissection_size17_12169_r1_c6.png?w=1100&#038;ssl=1" data-recalc-dims="1" /><img src="https://i0.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/perfect_dissection_size17_12169_r5_c6.png?w=1100&#038;ssl=1" data-recalc-dims="1" /> 

<img src="https://i0.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/perfect_dissection_size17_3091_r2_c4.png?w=1100&#038;ssl=1" data-recalc-dims="1" /><img src="https://i2.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/perfect_dissection_size17_3091_r3_c0.png?w=1100&#038;ssl=1" data-recalc-dims="1" /> 

<img src="https://i1.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/perfect_dissection_size17_3095_r0_c2.png?w=1100&#038;ssl=1" data-recalc-dims="1" /><img src="https://i2.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/perfect_dissection_size17_3095_r1_c3.png?w=1100&#038;ssl=1" data-recalc-dims="1" /> 

Those graphics are produced using [PyX](http://pyx.sourceforge.net/) and [Sage](http://sagemath.org). Full PDFs are available [here](http://bitbucket.org/carlohamalainen/dissections/get/tip.zip).