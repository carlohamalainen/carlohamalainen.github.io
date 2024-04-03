---
author: Carlo Hamalainen

date: "2017-08-17T10:30:29Z"
guid: http://carlo-hamalainen.net/?p=945
title: Ubuntu 17 -- device not managed
url: /2017/08/17/ubuntu-17-device-not-managed/
---
I plugged in a D-Link DUB-1312 to my laptop running Ubuntu Zesty but Network Manager said that the interface was "not managed".

The fix, found [here](https://askubuntu.com/questions/882806/ethernet-device-not-managed), is to remove the contents of one file. Better to save the original file and touch an empty one:

```
$ sudo mv    /usr/lib/NetworkManager/conf.d/10-globally-managed-devices.conf{,_ORIGINAL}
$ sudo touch /usr/lib/NetworkManager/conf.d/10-globally-managed-devices.conf
```

For reference, here's the info about the DUB-1312 USB ethernet adapter:

```
$ sudo apt update
$ sudo apt install hwinfo
$ sudo hwinfo --netcard

(other output snipped)

40: USB 00.0: 0200 Ethernet controller
  [Created at usb.122]
  Unique ID: VQs5.d0KcpDt5qE6
  Parent ID: 75L1.MLPSY0FvjsF
  SysFS ID: /devices/pci0000:00/0000:00:14.0/usb2/2-6/2-6.4/2-6.4.3/2-6.4.3:1.0
  SysFS BusID: 2-6.4.3:1.0
  Hardware Class: network
  Model: "D-Link DUB-1312"
  Hotplug: USB
  Vendor: usb 0x2001 "D-Link"
  Device: usb 0x4a00 "D-Link DUB-1312"
  Revision: "1.00"
  Serial ID: "000000000005FA"
  Driver: "ax88179_178a"
  Driver Modules: "ax88179_178a"
  Device File: enxe46f13f4be18
  HW Address: e4:6f:13:f4:be:18
  Permanent HW Address: e4:6f:13:f4:be:18
  Link detected: yes
  Module Alias: "usb:v2001p4A00d0100dcFFdscFFdp00icFFiscFFip00in00"
  Driver Info #0:
    Driver Status: ax88179_178a is active
    Driver Activation Cmd: "modprobe ax88179_178a"
  Config Status: cfg=new, avail=yes, need=no, active=unknown
  Attached to: #33 (Hub)
```
