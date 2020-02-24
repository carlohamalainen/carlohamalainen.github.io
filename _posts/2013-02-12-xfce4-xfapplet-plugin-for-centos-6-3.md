---
id: 695
title: Xfce4-xfapplet-plugin for Centos 6.3
date: 2013-02-12T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/02/12/xfce4-xfapplet-plugin-for-centos-6-3/
permalink: /2013/02/12/xfce4-xfapplet-plugin-for-centos-6-3/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
For some reason [xfce4-xfapplet-plugin](http://goodies.xfce.org/projects/panel-plugins/xfce4-xfapplet-plugin) has been dropped from recent Centos/Fedora releases, as the spec file for xfce4-panel proclaims: 

<pre># xfce4-xfapplet-plugin isn't in F15
Provides:       xfce4-xfapplet-plugin%{?_isa} = 0.1.0-11
Obsoletes:      xfce4-xfapplet-plugin <= 0.1.0-10.fc15
</pre>

This makes it basically impossible for anyone to use a Gnome2 applet on Xfce4. As a temporary work-around I have built an RPM of xfapplet-plugin on Centos 6.3 with the release number of 0.1.0-15, which gets around the Provides/Obsoletes lines: 

[xfce4-xfapplet-plugin-0.1.0-15.el6.x86_64.rpm](https://github.com/carlohamalainen/xfce4-xfapplet-centos6/blob/master/RPMS/x86_64/xfce4-xfapplet-plugin-0.1.0-15.el6.x86_64.rpm?raw=true) 

The git repo is here: <https://github.com/carlohamalainen/xfce4-xfapplet-centos6>.