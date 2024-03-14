---
author: Carlo Hamalainen

date: "2013-04-17T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2013/04/17/note-to-self-quickly-plot-a-shapefile-using-python-and-basemap/
id: 786
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: 'Note to self: quickly plot a shapefile using Python and basemap'
url: /2013/04/17/note-to-self-quickly-plot-a-shapefile-using-python-and-basemap/
---

```python
"""
Given a directory of shapefiles, e.g.

$ ls /path/to/shapefiles/
Project_Name.dbf
Project_Name.prj
Project_Name.sbn
Project_Name.sbx
Project_Name.shp
Project_Name.shp.xml
Project_Name.shx

draw a polygon for each shape based on some attribute (here, whether the
'Status' is completed or in-progress.
"""

import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
from matplotlib.patches import Polygon

m = Basemap(llcrnrlon=130. ,llcrnrlat=-48, urcrnrlon=170. ,urcrnrlat=-10., resolution='h', area_thresh=10000)

m.drawcoastlines()
m.fillcontinents()
m.drawcountries(linewidth=2)
m.drawstates()

m.drawmapboundary(fill_color='aqua')

s = m.readshapefile('/path/to/shapefiles/Project_Name', 'Project_Name')

for xy, info in zip(m.Project_Name, m.Project_Name_info):
    if info['Status'] == 'Completed':
        poly = Polygon(xy, facecolor='red', alpha=0.4)
        plt.gca().add_patch(poly)
    elif info['Status'] == 'in progress':
        poly = Polygon(xy, facecolor='green', alpha=0.4)
        plt.gca().add_patch(poly)
    else:
        print info['Status']

plt.legend()
plt.show()
```
