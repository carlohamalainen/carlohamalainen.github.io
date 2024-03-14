---
author: Carlo Hamalainen

date: "2016-01-16T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2016/01/16/minc-interfaces-in-nipype/
id: 717
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: MINC interfaces in Nipype
url: /2016/01/16/minc-interfaces-in-nipype/
---
About two years ago I wrote [volgenmodel-nipype](https://github.com/carlohamalainen/volgenmodel-nipype), a port of [Andrew Janke's volgenmodel](https://github.com/andrewjanke/volgenmodel) to the [Nipype](https://github.com/nipy/nipype) workflow system. Nipype provides a framework for wrapping legacy command-line tools in a simple to use interface, which also plugs in to a [workflow engine](http://www.mit.edu/~satra/nipype-nightly/users/plugins.html) that can run jobs on a multicore PC, IPython parallel, SGE/PBS, etc. 

Using a workflow that takes care of naming and tracking input/output files is very convenient. To blur an image (using mincblur) one can create a Node with the Blur interface, and then use .connect to send the output of some other node into this node: 

```python
blur = pe.Node(interface=Blur(fwhm=step_x*15),
                              name='blur_' + snum_txt)

workflow.connect(norm, 'output_threshold_mask', blur, 'input_file')
```

When I first developed volgenmodel-nipype I wrote my own Nipype interfaces for quite a few [MINC](https://github.com/BIC-MNI/minc-toolkit) tools. Over the 2015 Xmas holidays I got those interfaces [merged](https://github.com/nipy/nipype/pull/1304) into the master branch of Nipype. 

I took this opportunity to tidy up volgenmodel-nipype. There are no locally defined MINC interfaces. I added a public dataset, available in a separate repository: <https://github.com/carlohamalainen/volgenmodel-fast-example>. Previously this data wasn't publicly available. I also added [some Docker scripts](https://github.com/carlohamalainen/volgenmodel-nipype/tree/master/docker) to run the whole workflow and compare the result against a known good model output, which I run in a weekly cronjob as a poor-person's continuous integration test suite. 

The mouse brain sample data produces a model that looks like this: 

{{< figure src="https://raw.githubusercontent.com/carlohamalainen/volgenmodel-fast-example/master/model-2016-01-09.png" >}}
