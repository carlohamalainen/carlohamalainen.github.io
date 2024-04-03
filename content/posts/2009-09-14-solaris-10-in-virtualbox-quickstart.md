---
author: Carlo Hamalainen

date: "2009-09-14T00:00:00Z"
format: image
title: Solaris 10 in VirtualBox quickstart
url: /2009/09/14/solaris-10-in-virtualbox-quickstart/
---
How to install Solaris 10 in VirtualBox with an Ubuntu 9.04 host.

First, [download the installer DVD](http://www.sun.com/software/solaris/). Make a VirtualBox instance of type Solaris, mount the ISO file, and do the install. It's all fairly straightforward (say yes to all the defaults).

You will probably want to be able to ssh to your Solaris installation. To do this, assuming that your VirtualBox instance is called Solaris10, run these commands in a terminal (taken from [here](http://kdl.nobugware.com/post/2009/02/17/virtualbox-nat-ssh-guest/)):

    VBoxManage setextradata Solaris10  "VBoxInternal/Devices/e1000/0/LUN#0/Config/ssh/Protocol" TCP
    VBoxManage setextradata Solaris10  "VBoxInternal/Devices/e1000/0/LUN#0/Config/ssh/GuestPort" 22
    VBoxManage setextradata Solaris10  "VBoxInternal/Devices/e1000/0/LUN#0/Config/ssh/HostPort" 2222

Check that the redirect has been added (the last 3 lines):

    $ VBoxManage getextradata Solaris10 enumerate
    VirtualBox Command Line Management Interface Version 3.0.6
    (C) 2005-2009 Sun Microsystems, Inc.
    All rights reserved.

    Key: GUI/SaveMountedAtRuntime, Value: yes
    Key: GUI/ShowMiniToolBar, Value: yes
    Key: GUI/MiniToolBarAlignment, Value: bottom
    Key: GUI/LastCloseAction, Value: save
    Key: GUI/LastWindowPostion, Value: 4,-33,1016,522,max
    Key: GUI/Fullscreen, Value: off
    Key: GUI/Seamless, Value: off
    Key: GUI/AutoresizeGuest, Value: on
    Key: GUI/MiniToolBarAutoHide, Value: on
    Key: VBoxInternal/Devices/e1000/0/LUN#0/Config/ssh/Protocol, Value: TCP
    Key: VBoxInternal/Devices/e1000/0/LUN#0/Config/ssh/GuestPort, Value: 22
    Key: VBoxInternal/Devices/e1000/0/LUN#0/Config/ssh/HostPort, Value: 2222

This will redirect port 2222 on localhost to port 22 in the guest. In other words,

    ssh -p 2222 user@localhost

will log you in. Note that an initial install of Solaris 10 will have only the root account, and by default root is not allowed to log in via ssh. So open a terminal in Solaris and add a user (-g staff makes this an administrator):

    # useradd  -d /export/home/username -g staff -m -s /bin/bash username
    # passwd username
    (set the password here)

One final note: if you need to edit ssh options in /etc/ssh/, there is no /etc/init.d/sshd command to restart sshd. Instead use 

    svcadm restart ssh

It works:

    $ ssh -p 2222 localhost
    Password:
    Last login: Mon Sep 14 14:34:09 2009 from 10.0.2.2
    Sun Microsystems Inc.   SunOS 5.10      Generic January 2005
    -bash-3.00$ uname -a
    SunOS unknown 5.10 Generic_139556-08 i86pc i386 i86pc
    -bash-3.00$

A quick and easy way to get to the Solaris filesystem from the host is to use sshfs:

    sudo aptitude install sshfs
    sudo adduser username fuse

Log out and log in to activate the group change.

Now mount the Solaris instance in your home directory:

    mkdir $HOME/solaris
    sshfs username@localhost:/ $HOME/solaris/ -p 2222

I find that sshfs is very handy for working with remote systems.
