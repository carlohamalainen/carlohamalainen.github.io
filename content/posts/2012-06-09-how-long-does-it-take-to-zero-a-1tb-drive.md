---
author: Carlo Hamalainen

date: "2012-06-09T00:00:00Z"
format: image
title: How long does it take to zero a 1Tb drive?
url: /2012/06/09/how-long-does-it-take-to-zero-a-1tb-drive/
---
About 10 hours for a USB2 external drive: 

    root@bobcat:~# time dd if=/dev/zero of=/dev/disk/by-id/usb-TOSHIBA_MK1059GSM_0FA6FFFFFFFF-0:0-part1 bs=4M

    dd: writing `/dev/disk/by-id/usb-TOSHIBA_MK1059GSM_0FA6FFFFFFFF-0:0-part1': No space left on device
    238467+0 records in
    238466+0 records out
    1000202241024 bytes (1.0 TB) copied, 37410.6 s, 26.7 MB/s

    real    623m30.583s
    user    0m3.416s
    sys     77m19.798s
