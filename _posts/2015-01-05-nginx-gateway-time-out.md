---
id: 811
title: Nginx gateway time-out
date: 2015-01-05T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2015/01/05/nginx-gateway-time-out/
permalink: /2015/01/05/nginx-gateway-time-out/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Occasionally I get a "504 Gateway Time-out" from nginx in conjunction with php-fastcgi. Restarting php-fcgi has no effect (Debian wheezy amd64). 

Stop php-fcgi completely: 

```
# /etc/init.d/php-fcgi stop
Stopping PHP FastCGI: php-cgi.
```

But php-cgi processes are still running! 

```
# ps aux | grep php
www-data  2858  0.0  0.2 101188  3920 ?        Ss    2014   0:02 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 13927  0.0  1.5 123892 25736 ?        S     2014   0:40 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14036  0.0  1.5 123908 25744 ?        S     2014   0:51 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14067  0.0  1.5 123856 25700 ?        S     2014   0:43 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14185  0.0  1.5 124248 26104 ?        S     2014   0:56 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14196  0.0  1.5 123856 25696 ?        S     2014   0:27 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14215  0.0  1.5 123900 25740 ?        S     2014   0:46 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14222  0.0  1.5 124104 25980 ?        S     2014   0:45 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14234  0.0  1.5 124148 26004 ?        S     2014   0:22 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14237  0.0  1.5 123920 25780 ?        S     2014   0:53 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14240  0.0  1.5 124124 25992 ?        S     2014   1:08 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14275  0.0  1.5 123888 25748 ?        S     2014   0:31 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14281  0.0  1.4 123856 25320 ?        S     2014   2:27 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14333  0.0  1.5 123860 25744 ?        S     2014   1:36 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14347  0.0  1.5 124152 25992 ?        S     2014   0:21 /usr/bin/php-cgi -b 127.0.0.1:9000
www-data 14356  0.0  1.5 124436 26300 ?        S     2014   0:25 /usr/bin/php-cgi -b 127.0.0.1:9000
```

So kill them manually and then restart php-fcgi: 

```
# pkill -9 php-cgi

# ps aux | grep php
root     30663  0.0  0.0 114676   880 pts/0    S+   06:00   0:00 grep php

# /etc/init.d/php-fcgi restart
```

Then it works as before.
