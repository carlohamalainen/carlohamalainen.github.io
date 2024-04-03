---
author: Carlo Hamalainen

date: "2015-06-03T00:00:00Z"
format: image
title: 'DCMTK: No presentation context for: (unknown SOP class) 1.3.12.2.1107.5.9.1'
url: /2015/06/03/dcmtk-no-presentation-context-for-unknown-sop-class-1-3-12-2-1107-5-9-1/
---
I had a DCMTK DICOM server running with the command 

```
storescp -p 7000 -v --fork -fe '.IMA' --sort-on-study-uid 'my_prefix'
```

One of our Siemens instruments couldn't push some files to the DCMTK server, so I tried to send the files manually using storescu: 

```
parallel -j 30 storescu localhost 7000 -- `find siemens_data/ -type f`
```

While the Siemens instrument gave nothing of note, I found that storescu failed with:

```
E: No presentation context for: (unknown SOP class) 1.3.12.2.1107.5.9.1
E: Store SCU Failed: 0006:0208 DIMSE No valid Presentation Context ID
```

Apparently 1.3.12.2.1107.5.9.1 is a "private" Siemens SOP class. To fix things in my situation I tried adding the -pm flag so that storescp would accept unknown SOP classes: 

```
storescp -p 7000 -v --fork -pm -fe '.IMA' --sort-on-study-uid 'my_prefix'
```

This fixed things so that the Siemens instrument could send data, but storescu still failed. It turned out, after reading [this forum post](http://forum.dcmtk.org/viewtopic.php?f=1&t=2227&view=previous), that storescu needs to know about the new SOP class. 

I copied /etc/dcmtk/storescu.cfg and edited out the 125th presentation context definition and added the Siemens UID: 

```
# PresentationContext125 = XRayRadiationDoseSRStorageUncompressed
PresentationContext125 = 1.3.12.2.1107.5.9.1Uncompressed
```

There are already 128 presentation contexts in the file and you can't have more than that (some limitation in DICOM?). Now  
pushing files using storescu works, if we refer to the config file and the Default profile entry: 

```
parallel -j 30 storescu -xf storescu.cfg Default localhost 7000 -- `find extract/ -type f`
```

The [forum post](http://forum.dcmtk.org/viewtopic.php?f=1&t=2227&view=previous) that I linked to earlier has an example config file with a [PrivateSiemens] section in the [[Profiles]]. I tried this but (as far as I understand) you have to copy all of the SOP classes that you might see, and run storescu referring to the PrivateSiemens profile name. So you may as well edit out an unused presentation context in the Default configuration. 

**Archived Comments**

Date: 2015-07-23 18:29:35.372382 UTC

Author: J. Riesmeier

You could also give the DCMTK command line tool "dcmsend" a try: <http://blog.jriesmeier.com/2011/10/sending-dicom-files-more-easily/>. 

Date: 2016-11-01 00:30:26.567742 UTC

Author: Tom C

Thanks Carlo, this solved my problem!

Date: 2017-03-01 04:58:52.305799 UTC

Author: Ashok B

Thanks Carlo.

It worked for me 🙂
