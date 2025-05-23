---
author: Carlo Hamalainen
date: "2009-03-25T00:00:00Z"
format: image
title: Hello world with pydesign -- STS(13) and STS(15)
url: /2009/03/25/hello-world-with-pydesign-sts13-and-sts15/
---

I wanted a listing of the blocks of the 2 nonisomorphic STS (Steiner Triple System) on 13 points and the 80 nonisomorphic STS on 15 points. Over at [designtheory.org](http://designtheory.org) there is a [database of t-designs](http://designtheory.org/database/t-designs/). An STS(13) corresponds to a t-design t=2, v=13, b=26, r=6, k=3, L=1 and an STS(15) corresponds to a t-design t=2, v=15, b=35, r=7, k=3, L1. Here is some code that uses [pydesign](http://designtheory.org/software/pydesign/) to read the blocks from DTRS external representation files:

```python
from pydesign import ext_rep
from pydesign import block_design

STS = {}

def save_blocks(t):
    b = block_design.BaseBlockDesign(ext_rep.XTree(t))
    if not STS.has_key(b.v): STS[b.v] = []
    STS[b.v].append(b.blocks)

proc = ext_rep.XTreeProcessor()
proc.block_design_proc = save_blocks

f = ext_rep.open_extrep_file('t2-v13-b26-r6-k3-L1.icgsa.txt.bz2')
proc.parse(f)
f.close()

f = ext_rep.open_extrep_file('t2-v15-b35-r7-k3-L1.icgsa.txt.bz2')
proc.parse(f)
f.close()

assert len(STS[13]) == 2
assert len(STS[15]) == 80

print "An STS(13):", STS[13][0]
```

**Archived Comments**

Date: 2009-05-11 18:10:46 UTC

Author: david joyner

Why don't you add this to Sage? Sage's incidence structure class is based on pydesign but the database read-write functionality was left out. Please feel free to add it back in and ask me or Dan Gordon to referee.
