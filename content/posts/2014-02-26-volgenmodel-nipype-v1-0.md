---
author: Carlo Hamalainen

date: "2014-02-26T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2014/02/26/volgenmodel-nipype-v1-0/
id: 699
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: volgenmodel-nipype v1.0
url: /2014/02/26/volgenmodel-nipype-v1-0/
---
Here is my latest project: <https://github.com/carlohamalainen/volgenmodel-nipype>. It is a port of the [Perl script volgenmodel](https://github.com/andrewjanke/volgenmodel) to Python, using the functionality of [Nipype](https://github.com/nipy/nipype). 

A lot of scientific workflow code has a common pattern, something like this: collect some input files, run something to produce intermediate results, and then combine the results into a final result. One way to implement the workflow is to glob the files and set up arrays or dictionaries to keep track of the outputs. 

```python
files = glob.glob('/tmp/blah*.dat')

intermediate_result = [None] * len(files)

for (i, f) in enumerate(files):
    intermediate_result[i] = fn1(f, param=0.3)

final_result = fn2(intermediate_result)
```

The problem with this approach is that it doesn't scale well nor is it easy to reason about. The equivalent in Nipype is: 

```python
import nipype.pipeline.engine as pe
import nipype.interfaces.io as nio

datasource = pe.Node(interface=nio.DataGrabber(sort_filelist=True), name='datasource_dat')
datasource.inputs.base_directory = '/scratch/data'
datasource.inputs.template = 'blah*.dat'

datasink = pe.Node(interface=nio.DataSink(), name="datasink")
datasink.inputs.base_directory = '/scratch/output'

intermediate = pe.MapNode(
                    interface=fn1_interface(param=0.3)
                    name='intermediate_mapnode',
                    iterfield=['input_file'])

final = pe.Node(
            interface=fn2,
            name='final_node')

workflow = pe.Workflow(name="workflow")

# Apply the fn1 interface to each file in the datasource:
workflow.connect(datasource, 'outfiles', intermediate, 'input_file')

# Apply the fn2 interface to the list of outputs from the intermediate map node:
workflow.connect(intermediate, 'output_file', final, 'input_file')

# Save the final output:
workflow.connect(final, 'output_file', datasink, 'final')
```

This code is much closer to the actual problem that we are trying to solve, and as a bonus we don't have to take care of arrays of input and output files, which is pure agony and prone to errors. 

Nipype lets us run the workflow using a single core like this: 

```python
workflow.run()
```

or we can fire it up using 4 cores using: 

```python
workflow.run(plugin='MultiProc', plugin_args={'n_procs' : 4})
```

Nipype also has [plugins](http://nipy.org/nipype/users/plugins.html) for SGE, PBS, HTCondor, LSF, SLURM, and others. 

Here is volgenmodel-nipype's workflow graph (generating this graph is a one-liner with the workflow object). Click the image for the full size version. 

{{< figure src="https://github.com/carlohamalainen/volgenmodel-nipype/raw/master/volgenmodel_graph.png" >}}
