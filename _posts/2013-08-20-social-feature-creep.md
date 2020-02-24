---
id: 759
title: Social feature creep
date: 2013-08-20T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/08/20/social-feature-creep/
permalink: /2013/08/20/social-feature-creep/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Back in 2006 I got a gmail account. It was fantastic. I could get to my email from any PC with a web browser, I never had to worry about backups (I assumed that Google could do a better job than me), and storage was effectively unlimited. Compared to institutional email at the time, which usually had mail quotas like 20Mb, going with gmail was a no-brainer. 

Over time, though, things have changed. Gmail used to be just email, but now that Google is doing the whole "social" thing, my Google account is a whole lot of things: 

<img src="https://i1.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/blogdata/x-2013-08/google_takeout_list.png?w=1100&#038;ssl=1" data-recalc-dims="1" /> 

I can't see this changing, especially with players like Facebook in the marketplace. Some people do want a basic email service, or one that offers encrypted storage, but these are [under attack in the US](http://www.democracynow.org/2013/8/13/exclusive_owner_of_snowdens_email_service):

> "We've seen a lot of demand for, you know, people who want email but don’t necessarily want it lumped in and profiled along with their searches or their browsing history or any of their other Internet activities." (Ladar Levison, interview on Democracy Now) 

I'm not sure how we should respond to the larger political and social problem here, but in the mean time I have deleted my 7 year old gmail account along with all the associated accounts (Youtube, Location History, Blogger, etc). I'm running my own mail server on a Debian Squeeze VM, which I set up by following the excellent [ISPmail tutorial](https://workaround.org/ispmail/squeeze) at [workaround.org](https://workaround.org). Another small change is that I now use Firefox instead of Chrome, along with the [ghostery](https://addons.mozilla.org/en-US/firefox/addon/ghostery/) add-on. The first few days that I used ghostery were eye-opening. I never appreciated how many tracking things are embedded in websites that otherwise look normal. 

I found that the easiest way to extract all of my gmail messages was to use mpop. Here's my .mpoprc: 

<pre>defaults
tls on

account gmail
host pop.gmail.com
user USERNAME
password PASSWORD
keep on
only_new off
tls_starttls off
delivery mbox ~/gmail-backup/gmail.mbox

# Set a default account
account default : gmail
</pre>

Then to retrieve the mail: 

<pre>mpop --tls-certcheck=off -a
</pre>

To import it into Thunderbird, I used the [ImportExportTools add-on](https://addons.mozilla.org/en-US/thunderbird/addon/importexporttools/), which happily pulled in the 3.7Gb mbox file.