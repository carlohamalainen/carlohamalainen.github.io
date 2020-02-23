---
id: 703
title: Plot just Australia using NCL
date: 2012-06-13T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2012/06/13/plot-just-australia-using-ncl/
permalink: /2012/06/13/plot-just-australia-using-ncl/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Just a note to myself on plotting data over Australia using [NCL](http://www.ncl.ucar.edu/index.shtml) with only the coastal lines for Australia shown, using the low-res map:

<pre>; Plot data only over Australia (including Tasmania) using the default
; low-res database. Based on maponly_8.ncl from the NCL website.
;
; Carlo Hamalainen 2012-06-13

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin

  wks  = gsn_open_wks("png","maponly")

  res                           = True
  res@mpOutlineOn               = True
  res@mpFillOn                  = False

  res@mpOutlineBoundarySets     = "NoBoundaries"
  res@mpDataBaseVersion         = "LowRes"

  res@mpMinLatF = -60
  res@mpMaxLatF = 0
  res@mpMinLonF = 100
  res@mpMaxLonF = 165

  res@mpOutlineSpecifiers = (/"Australia", "Australia-Tasmania"/)
  plot = gsn_csm_map_ce(wks,res)
end
</pre>

Output: 

<img src="https://i0.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/maponly.png?w=1100&#038;ssl=1" data-recalc-dims="1" />