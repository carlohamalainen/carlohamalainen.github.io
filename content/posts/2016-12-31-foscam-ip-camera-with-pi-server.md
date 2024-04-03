---
author: Carlo Hamalainen

date: "2016-12-31T00:00:00Z"
format: image
title: Foscam IP camera with Pi server
url: /2016/12/31/foscam-ip-camera-with-pi-server/
---
Over the xmas holidays I set up a security camera for my parents. I used a [Foscam FI9805E IP camera](http://www.foscamaustralia.com.au/Foscam-POE-FI9805E-30M-H264-720P-HD-Wireless-IP-Camera-Outdoor-Silver-Infrared-Cut-Filter-2yr-Warranty) with a Raspberry Pi (model 3) as the server. An old 1Tb USB hard drive provides ample storage on the Pi. Here is the layout:

{{< figure src="/s3/oldblog/blogdata/medium/2016-12-18%2B%2B19-25-40.jpg" >}} 

The camera supports power over ethernet but it comes with an AC adaptor so you can use a normal ethernet setup as I have done.

## Shopping list

These are the bits that I bought:

  * Raspberry Pi model 3. Mine came with free [heatsinks](http://www.ebay.com.au/itm/291696505888?_trksid=p2060353.m2749.l2649&ssPageName=STRK%3AMEBIDX%3AIT).
  * Micro-SD card for the Pi's root file system.
  * 2.4amp AC to USB adaptor; powers the Pi via a micro-USB cable.
  * Foscam FI9805E IP camera.
  * Standard ethernet cable.

## External installation

{{< figure src="/s3/oldblog/blogdata/medium/2016-12-20%2B%2B12-00-28.jpg" >}} 

I used some [Sugru](https://sugru.com/) to keep the main cable straight against the roller-door track:

{{< figure src="/s3/oldblog/blogdata/medium/2016-12-20%2B%2B12-01-48.jpg" >}} 

Due to the garage being hot I mounted an old peg basket using picture frame hooks. Free flow of air is good.

{{< figure src="/s3/oldblog/blogdata/medium/2016-12-20%2B%2B12-00-47.jpg" >}} 

&nbsp;

## Setup

Unfortunately the camera's web interface requires a browser plugin to be installed (an EXE). Microsoft provides [free Windows virtual machine images](https://developer.microsoft.com/en-us/microsoft-edge/tools/vms/) which is handy.

For security reasons I have the camera on its own 192.168.1.x subnet, with the Pi running a DHCP server to give the camera a static IP. There is no route from 192.168.1.x to the outside world so I also run an ntp and ftp server on the Pi. Meanwhile, the Pi connects via wifi the normal 10.1.1.x network.

To view the camera's web interface via the Pi, I have [socat](http://www.dest-unreach.org/socat/) passing 8088 (on 10.1.1.x) to the 192.168.1.x network. I keep these going with supervisor:

```
$ cat /etc/supervisor/conf.d/socatcamera.conf
[program:socatcameratcp]
command=socat TCP-LISTEN:8088,fork TCP:192.168.1.33:88
directory=/tmp
autostart=true
autorestart=true
startretries=3
stderr_logfile=/var/log/socatcamera/socatcamera_tcp.err.log
stdout_logfile=/var/log/socatcamera/socatcamera_tcp.out.log
user=root

[program:socatcameraudp]
command=socat UDP-LISTEN:8088,fork UDP:192.168.1.33:88
directory=/tmp
autostart=true
autorestart=true
startretries=3
stderr_logfile=/var/log/socatcamera/socatcamera_udp.err.log
stdout_logfile=/var/log/socatcamera/socatcamera_udp.out.log
user=root
```

(TCP is probably enough, maybe the UDP isn't needed.)

Then I can view the live video stream using vlc (the Pi is 10.1.1.220):

```
vlc rtsp://user:pass@10.1.1.220:8088/videoMain
```

The camera has motion detection software and can store snapshots to an FTP server. For this I have vsftpd on the Pi. The snapshot files have names like MDAlarm_20161220-195958.jpg so it's easy to parse the year, month, day, time. A small Python script archives snapshots to an archive directory, and another script makes a html index (by day) which is served up via nginx.

For maintenance I have ssh access to the Pi from outside, with [rate limited ssh](https://www.rackaid.com/blog/how-to-block-ssh-brute-force-attacks/).

## Monitoring

The Pi logs its CPU temperature to my [Postgresql structured logging server](https://carlo-hamalainen.net/blog/2016/4/27/structured-logging) and a daily email report plots the temperatures for me:

{{< figure src="/s3/oldblog/blogdata/x-2016-12/calampi_temps.png" >}}

For real-time monitoring (5 minute intervals) I use a free account on [UptimeRobot](https://uptimerobot.com).

With the most aggressive settings for motion detection (smallest detection window and largest save window) we are seeing between 50 and 200Mb of snapshot images per 24 hour period.

## Bonus

Having a Pi on NBN fibre in Australia is convenient for running [WebDL](https://bitbucket.org/delx/webdl) for grabbing ABC and SBS shows.

## Update 2017-07-31

I gave up on the Rasperry Pi because it locked solid almost every day, requiring a hard reset. Dodgy SD Card? Who knows.

The replacement is a bottom end [Intel NUC](https://www.intel.sg/content/www/xa/en/products/boards-kits/nuc/kits/nuc5cpyh.html) with a 500Gb laptop SATA drive. It's a proper mini-PC so you can run 64bit Ubuntu, has a cooling fan, wifi, ethernet, bluetooth, etc.

{{< figure src="/wp-old/uploads/2016/12/2017-06-2314-21-49_000000.jpg" >}}

{{< figure src="/wp-old/uploads/2016/12/2017-06-2314-21-56_000000.jpg" >}}
