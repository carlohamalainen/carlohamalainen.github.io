---
id: 697
title: Best screenrc
date: 2010-06-30T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2010/06/30/best-screenrc/
permalink: /2010/06/30/best-screenrc/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
A mashup of nice options that I&#8217;ve accumulated over time. The status line is rather long but selecting it and copy &#8216;n&#8217; paste should work.

<pre># Ctrl-right square bracket doesn't seem to clash with anything:
escape ^]]

# Nice status at the bottom of the screen, permanently shown:
hardstatus alwayslastline '%{= kG}[ %{G}%H %{g}][%= %{= kw}%?%-Lw%?%{r}(%{W}%n*%f%t%?(%u)%?%{r})%{w}%?%+Lw%?%?%= %{g}][%{B} %d/%m %{W}%c %{g}]'

# Use Ctrl-left arrow and Ctrl-right arrow to cycle through windows.
bindkey ^[[1;5D prev
bindkey ^[[1;5C next
</pre>