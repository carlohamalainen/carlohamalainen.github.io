---
id: 804
title: ipywidgets demo
date: 2014-02-28T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/02/28/ipywidgets-demo/
permalink: /2014/02/28/ipywidgets-demo/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
 **This example has bitrotted &#8211; see the [Bokeh slider](https://carlo-hamalainen.net/blog/2015/7/31/bokeh-slider-for-phenology-of-two-interdependent-traits-in-migratory-birds-in-response-to-climate-change) instead.** 



My partner [Nadiah](http://nadiah.org) has developed  
an eco-evolutionary model describing the response of a migratory  
bird&#8217;s arrival time and prelaying period to climate change. The Octave code is here:  
<https://github.com/nadiahpk/phenology-two-trait-migratory-bird>.

The final plots look like this: 

<img src="https://i2.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/blogdata/x-2014-02/single-solution-two-trait-model.png?w=1100&#038;ssl=1" data-recalc-dims="1" /> 

A natural question is: how does the shape of the arrival time change as the main model parameter is varied? A nice way to visualise this is using [ipywidgets](https://github.com/jakevdp/ipywidgets) and [nbviewer](https://github.com/ipython/nbviewer). Here is an ipython notebook with a slider for the main model parameter: 

Getting this to work was surprisingly straightforward. In short: 

1. Make a notebook using ipython. I used [oct2py](https://pypi.python.org/pypi/oct2py) to call the  
Octave code from Python. 

2. Install nbviewer on a publicly accessible host. 

3. Run nbviewer like so:

<pre>cd $HOME/nbviewer # this is the nbviewer repository from github
python -m nbviewer --debug --no-cache --localfiles=$HOME/phenology-two-trait-migratory-bird
</pre>

This runs the server on port 5000. You could run it in a screen or tmux session, or use  
[supervisord](http://supervisord.org/) or [angel](https://hackage.haskell.org/package/angel) to keep the process alive.

4. Point nginx to the nbviewer proces: 

<pre># /etc/nginx/sites-available/amazonaws.com

server {

        listen   80;

        server_name ec2-xyz.amazonaws.com;

        access_log  /var/log/nginx/amazonaws.com.access.log;

        location / {
            proxy_pass http://127.0.0.1:5000; # Reverse proxy to nbviewer
        }

}
</pre>

5. The notebook doesn&#8217;t appear on nbviewer&#8217;s front page, so just naviate to a URL of the form 

<pre>http://your.host.example.com/localfiles/foo.ipynb
</pre>

to see the notebook foo.ipynb. 

Here is a HTML iframe containing the nbviewer view of [the phenology-two-trait-migratory-bird notebook](https://github.com/nadiahpk/phenology-two-trait-migratory-bird).  
Try out the slider at the bottom. 

<!&#8211; [http://ec2-107-22-54-51.compute-1.amazonaws.com/localfile/arrival\_times\_notebook.ipynb](http://ec2-107-22-54-51.compute-1.amazonaws.com/localfile/arrival_times_notebook.ipynb) 

IPython development has really taken off recently; check out the SciPy 2013 keynote for more info: 

<div class="jetpack-video-wrapper">
  <span class="embed-youtube" style="text-align:center; display: block;"></span>
</div>