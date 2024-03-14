---
author: Carlo Hamalainen
date: "2010-06-18T00:00:00Z"
guid: http://carlo-hamalainen.net/2010/06/18/recovery-of-data-from-a-raid-5-disk/
title: Recovery of data from a RAID 5 disk
url: /2010/06/18/recovery-of-data-from-a-raid-5-disk/
---
At work I replaced a single drive in a HP DL380 RAID 5 array. The drive was only giving SMART 1720 errors (imminent failure) and HP wanted the drive back due to warranty conditions, so the question came up of erasing any data on the drive.

I got conflicting advice -- some people said that you would definitely be able to read data off the drive, and other people said that because it was part of a RAID 5 array, it was impossible to reconstruct the array, so the drive could be sent back with no worries. I decided to test how much data could be read from a single drive from a three disk RAID 5 array.

Details of the server: HP DL380, Smart Array P400 controller. The RAID 5 array was configured with two logical volumes.

Details of the drive:

    72GB 2.5" Serial Attached SCSI (SAS) SFF
    Single Port Hot-Plug 15K HDD
    Option Part# 431935-B21
    Spare Part# 432321-001
    Assembly Part# 431930-002
    Model# DH072ABAA6

Due to physical access issues the drive was taken out of the server and installed in a standard desktop PC running Windows XP with a Promise FastTrak TX2650 SAS controller card. After installing the TX2650 drivers the SAS drive was recognised as a standard hard drive using JBOD, so it immediately appeared as a logical drive in Windows XP. Here's the card and drive (fortunately the TX2650 comes with all the cables that you need):

{{< figure src="/stuff/wp-content/uploads/2010/06/card_and_drive.png" >}}

I ran [PhotoRec](http://www.cgsecurity.org/wiki/PhotoRec) directly on the SAS drive:

{{< figure src="/stuff/wp-content/uploads/2010/06/photorec1.png" >}}

After about two hours PhotoRec finished:

{{< figure src="/stuff/wp-content/uploads/2010/06/photorec2.png" >}}

Those recovered files total about 8Gb (the original RAID 5 array contained about 50Gb of data). From our perspective, the best hit is searching for a certain prefix "PATNOK" that we use in files for daily demographics imports:

{{< figure src="/stuff/wp-content/uploads/2010/06/photorec-patnok.png" >}}

Each of those 1443 files contains _at least_ one set of patient details (name, address, Medicare number, date of birth, phone number, next of kin, next of kin contact details).

That's a clear example of sensitive data coming off a single drive from a RAID 5 array.

The success rate for larger files was pretty low, I suspect due to the fact that data is striped on a RAID 5 disk.

**Archived Comments**

Date: 2012-08-22 11:20:16 UTC

Author: Pgt

Thanks for sharing

Date: 2013-08-11 20:10:36.618802 UTC

Author: Escondido

Thanks Carlo, this is a great test and great information. Did you actually open up any of the text files to confirm the data was intact?

Date: 2013-08-11 20:20:03.180561 UTC

Author: Carlo

Yes, they were intact. From memory, and this is a few years ago, I ran [shred](http://en.wikipedia.org/wiki/Shred_%28Unix%29) on the nearly dead drive to wipe it.

Date: 2013-10-06 12:34:11.421942 UTC

Author: SAM

Thanks man for this wonderful information, U helped me a lot with ur test, I'm having a dell server with SAS raid 5 configuration, one of the hard drivers got failed, by mistake they did something prevented us to complete the normal procedure of replacing the hard drive and to continue with the standard raid 5 features as we all know, so now I'm having the tow remained SAS hard drives & was looking for a way of how to read the contained data till I read ur post here, and as I understood that I can buy the Promise FastTrak TX2650 SAS controller card to attach both hard drives on it to be able to retrieve data,,

Thank U very much and hope I get my data back,,

Date: 2014-03-21 07:47:48.079679 UTC

Author: KETAN

Thanks for sharing Carlo , its huge security risk . I was searching online for quite sometime and fortunately your experiment will help many to conclude that they need additional security to protect data. Kind regards

Date: 2016-08-21 10:12:03.833736 UTC

Author: johnstone 

Hey

This looks good information. I would like to know if you were able to retrieve your data from all the disks. another thing does this card work with all types of SAS drives?
