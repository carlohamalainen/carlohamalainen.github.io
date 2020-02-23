---
id: 781
title: 'Note to self: clone remote branch using git'
date: 2012-11-02T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2012/11/02/note-to-self-clone-remote-branch-using-git/
permalink: /2012/11/02/note-to-self-clone-remote-branch-using-git/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
By default &#8220;git branch&#8221; doesn&#8217;t show the remote branches for a cloned repo, so use &#8220;git branch -a&#8221; to see all of them and then make a local branch with a suitable name:

<pre>$ git clone git@github.com:CVL-dev/cvl-fabric-launcher.git
Cloning into 'cvl-fabric-launcher'...
X11 forwarding request failed on channel 0
remote: Counting objects: 1527, done.
remote: Compressing objects: 100% (749/749), done.
remote: Total 1527 (delta 792), reused 1463 (delta 728)
Receiving objects: 100% (1527/1527), 6.28 MiB | 996 KiB/s, done.
Resolving deltas: 100% (792/792), done.

$ cd cvl-fabric-launcher/
$ git branch -a
* master
  remotes/origin/CVLFAB45_ssh_tunnel_check
  remotes/origin/HEAD -&gt; origin/master
  remotes/origin/TurboVNC_java_test
  remotes/origin/detailed_logging
  remotes/origin/master

$ git checkout -b detailed_logging remotes/origin/detailed_logging
Branch detailed_logging set up to track remote branch detailed_logging from origin.
Switched to a new branch 'detailed_logging'
</pre>

Credit: <http://stackoverflow.com/questions/67699/how-do-i-clone-all-remote-branches-with-git>.