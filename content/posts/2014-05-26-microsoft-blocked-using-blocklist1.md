---
author: Carlo Hamalainen

date: "2014-05-26T00:00:00Z"
format: image
title: Microsoft "blocked using Blocklist1"
url: /2014/05/26/microsoft-blocked-using-blocklist1/
---
I've been running my own mail host for a while now on an Amazon EC2 instance and haven't had any problems, but recently some mail bounced from a user whose organisation has hosted email with Microsoft Office365. Here's the bounce: 

```
-------- Original Message --------
Subject: Undelivered Mail Returned to Sender
Date: Thu, 10 Apr 2014 14:41:24 +0000 (UTC)
From: MAILER-DAEMON@example.com (Mail Delivery System)
To: carlo@example.net

This is the mail system at host example.com

I'm sorry to have to inform you that your message could not
be delivered to one or more recipients. It's attached below.

For further assistance, please send mail to postmaster.

If you do so, please include this problem report. You can
delete your own text from the attached returned message.

The mail system

: host zzzzzzzzz.mail.eo.outlook.com[xxx.xxx.xxx.xxx]
said: 550 5.7.1 Service unavailable; Client host [yyy.yy.yy.yy] blocked
using Blocklist 1; To request removal from this list please forward this
message to delist@messaging.microsoft.com (in reply to RCPT TO command)
```

I thought that this was pretty weird given that my host passed all the tests on MX Toolbox's [blacklist checker](http://mxtoolbox.com/blacklists.aspx). I fired off an email to Microsoft customer support saying "this is weird, remove my host from your blacklist" and after a few days I got this response: 

```
-------- Original Message --------
Subject:    RE: SRX1242320246ID - Fwd: Undelivered Mail Returned to Sender
Date:   Mon, 14 Apr 2014 19:06:25 +0000
From:   Microsoft Customer Support
To:     carlo@example.net

Hello ,

Thank you for contacting Microsoft Online Services Technical Support.
This email is in reference to ticket number zzzzzzzzzz, which was opened
in regards to your delisting request for xxx.xx.xx.xx .

The IP address you submitted has been reviewed and removed from our
block lists.  Please note that there may be a 1-2 hour delay before this
change propagates through our entire system.

We apologize for any inconvenience this may have caused you.  As long as
our spam filtering systems do not mark a majority of email from the IP
address as spam-like, your messages will be allowed to flow as normal
through our network.  However, should we detect an increase in spam-like
activity, the IP address may be re-added to our block list.

Should you have any further questions or concerns, please feel free to
respond to this email.

Thank you again for contacting Microsoft Online Services technical
support and giving us the opportunity to serve you.
```

After that everything seemed to be ok. 

While I was waiting for Microsoft to unblock me I tried Amazon's [Simple Email Service](https://aws.amazon.com/ses) which is free if you send less than 2000 emails per day. Email sent via the SES SMTPS host were not blocked by Microsoft. So if you run your own mail host on Amazon EC2 then it's probably a good idea to have SES enabled as a backup.
