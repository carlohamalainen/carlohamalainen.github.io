---
id: 735
title: Cheap standing desk
date: 2013-10-17T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/10/17/cheap-standing-desk/
permalink: /2013/10/17/cheap-standing-desk/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Is sitting all day in front of a computer ok as long as you exercise for an hour or two every day? Here's a quote from a Runner's World article titled [Sitting is the new smoking -- even for runners](http://www.runnersworld.com/health/sitting-is-the-new-smoking-even-for-runners?page=single) that indicates otherwise:

> "Up until very recently, if you exercised for 60 minutes or more a day, you were considered physically active, case closed," says Travis Saunders, a Ph.D. student and certified exercise physiologist at the Healthy Active Living and Obesity Research Group at Children's Hospital of Eastern Ontario. "Now a consistent body of emerging research suggests it is entirely possible to meet current physical activity guidelines while still being incredibly sedentary, and that sitting increases your risk of death and disease, even if you are getting plenty of physical activity. It's a bit like smoking. Smoking is bad for you even if you get lots of exercise. So is sitting too much."
> 
> ... 
> 
> Hamilton recently discovered that a key gene (called lipid phosphate phosphatase-1 or LPP1) that helps prevent blood clotting and inflammation to keep your cardiovascular system healthy is significantly suppressed when you sit for a few hours. "The shocker was that LPP1 was not impacted by exercise if the muscles were inactive most of the day," Hamilton says. "Pretty scary to say that LPP1 is sensitive to sitting but resistant to exercise." 

Standing desks are trendy at the moment but quite expensive, unless you find some wooden crates that are just the right size: 

<img border="0" src="https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/blogdata/medium/2013-10-15%2B%2B10-47-17.jpg?w=1100&ssl=1" alt="[photo]" data-recalc-dims="1" /> 

I'd like to see research quantifying the benefit of using a standing desk. Do I need to work at it all day? In 2 hour blocks with 30 minute sitting blocks? 

The Microsoft Natural keyboard is the best keyboard that I've ever used, and now seems to be a necessity because I get an odd icy feelin in my right wrist if I type for too long on a normal keyboard. Being Australian, I'm used to the US-English keymap, and I found that the Microsoft Natural is available from amazon.de in the right keymap: [Microsoft NaturalErgoTastatur 4000 USB (EN)](http://www.amazon.de/Microsoft-NaturalErgoTastatur-4000-USB-EN/dp/B000H4WIFY/ref=sr_1_1?s=computers&ie=UTF8&qid=1381828393&sr=1-1&keywords=Microsoft+NaturalErgoTastatur+4000+USB+%28EN%29). The "(EN") refers to US-English; for a British keyboard you would want "(GB)". 

One thing about the Microsoft Natural keyboard is that it's huge, and using a mouse right-handed would not be ergonomically sensible. So I got a Logitech Marble trackball which can be used with the left hand. I find that this setup works perfectly. 

<img border="0" src="https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/blogdata/medium/2013-10-15%2B%2B10-47-26.jpg?w=1100&ssl=1" alt="[photo]" data-recalc-dims="1" /> 

To swap the left and right buttons and make the small-right button do middle click (important for X Windows), use this xmodmap command: 

    xmodmap -e "pointer = 3 9 1 4 5 6 7 8 2"

It's possible to make the small left mouse button enable scroll wheel emulation on the Logitech Marble but I don't find this to be comfortable. More configuration advice is here: [https://help.ubuntu.com/community/Logitech\_Marblemouse\_USB](https://help.ubuntu.com/community/Logitech_Marblemouse_USB).
