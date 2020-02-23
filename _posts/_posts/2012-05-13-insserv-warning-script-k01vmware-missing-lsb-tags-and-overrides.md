---
id: 694
title: 'insserv: warning: script &#8216;K01vmware&#8217; missing LSB tags and overrides'
date: 2012-05-13T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2012/05/13/insserv-warning-script-k01vmware-missing-lsb-tags-and-overrides/
permalink: /2012/05/13/insserv-warning-script-k01vmware-missing-lsb-tags-and-overrides/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Note to self: if apt-get dist-upgrade explodes on Debian Squeeze with the error

<pre>Setting up initscripts (2.88dsf-13.1+squeeze1) ...
insserv: warning: script 'K01vmware' missing LSB tags and overrides
insserv: warning: script 'S50vmware-USBArbitrator' missing LSB tags and overrides
insserv: warning: script 'vmware-USBArbitrator' missing LSB tags and overrides
insserv: warning: script 'vmware' missing LSB tags and overrides
insserv: There is a loop between service rmnologin and mountnfs if started
insserv:  loop involving service mountnfs at depth 8

(more output snipped)
</pre>

Then follow the instructions of post rldleblanc at <http://communities.vmware.com/thread/337769>: 

<pre>rldleblanc
35 posts since
05-Aug-2005


7. 05-Dec-2011 11:14   in response to: SamSpade
Re: Vmware Player Prevents Aptitude from Installing Debian Packages
Probably a cleaner way to approach this is to use /etc/insserv/overrides.
Do the following:

Create /etc/insserv/overrides/vmware with the following:

### BEGIN INIT INFO
# Provides:          vmware
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 5
# Default-Stop:      2 3 5
# Short-Description: VMware VMX service for virtual machines
# Description:       Allows running of VMware virtual machines.
### END INIT INFO

Create /etc/insserv/overrides/vmware-USBArbitrator with the following:

### BEGIN INIT INFO
# Provides:          vmware-USBArbitrator
# Required-Start:    $remote_fs $syslog vmware
# Required-Stop:     $remote_fs $syslog vmware
# Default-Start:     2 3 5
# Default-Stop:      2 3 5
# Short-Description: Start daemon when vmware starts
# Description:       Enable service provided by daemon.
### END INIT INFO

Then run 'chmod +x /etc/insserv/overrides/vmware*

This prevents the fix from being broken with an update to the shipped init.d
script and my fix your USB problem. Since I don't use USB in a VM, I can't
test it.

Robert
</pre>