---
author: Carlo Hamalainen

date: "2010-06-06T00:00:00Z"
format: image
title: cu instead of minicom
url: /2010/06/06/cu-instead-of-minicom/
---
To connect to a SheevaPlug serial USB console, plug it in, and then use this one liner:

    cu -s 115200 -l /dev/ttyUSB0

Quicker than messing around with minicom and setting the device in a profile, etc. Thanks to [Wilfred on the computingplugs wiki](http://computingplugs.com/index.php/Connecting_to_the_serial_console).
