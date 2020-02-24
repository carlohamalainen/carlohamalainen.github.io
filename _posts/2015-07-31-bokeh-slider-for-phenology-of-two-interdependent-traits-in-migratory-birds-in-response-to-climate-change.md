---
id: 746
title: 'Bokeh slider for "Phenology of two interdependent traits in migratory birds in response to climate change"'
date: 2015-07-31T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2015/07/31/bokeh-slider-for-phenology-of-two-interdependent-traits-in-migratory-birds-in-response-to-climate-change/
permalink: /2015/07/31/bokeh-slider-for-phenology-of-two-interdependent-traits-in-migratory-birds-in-response-to-climate-change/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
(7 January 2017: some updates, see end of the post.) 

A while I ago I made a [slider using ipywidgets](http://carlo-hamalainen.net/blog/2014/2/28/ipywidgets-demo) that could be embedded in a html page (handy for blog posts). This week I decided to see where things were at with IPython or Jupyter. 

As of July 2015 the [ipywidgets](https://github.com/jakevdp/ipywidgets) package is unsupported. The author recommends using IPython's built-in interactive tools. However IPython doesn't have static widgets yet, according to [this issue](https://github.com/ipython/ipywidgets/issues/16). A [StackOverflow answer](http://stackoverflow.com/a/31505677) mentioned [Bokeh](http://bokeh.pydata.org/en/latest/) so I decided to give that a go. 

## Bokeh slider 

Here is a slider that replicates my earlier ipywidgets effort: 

<https://carlo-hamalainen.net/bokehslider/bokehslider2017>

This is pretty nice. It's an interactive slider, works on desktop and mobile, and doesn't have any of the notebook stuff around it. Just the graph with the interactive widget. Bokeh also provides tools for zooming and panning around. It's also worth mentioning that Bokeh provides a GUI library (things like hboxes, vboxes, layouts, etc) and my impression is that you could have multiple plots changing based on one slider, two plots tied together on some parameter, or whatever else you dreamt up. 

The slider is implemented in [bokehslider.py](https://github.com/carlohamalainen/phenology-two-trait-migratory-bird/blob/bokeh-slider/bokehslider.py) and is run using bokeh-server --ip 0.0.0.0 --script bokehslider.py. One strange thing that I ran into was that the slider wasn't interactive unless I opened up port 5006 on my server, even though Nginx is doing the proxy_pass stuff. I suspect that some of the Bokeh-generated Javascript expects to be able to connect to the host on 5006. 

Here's the relevant Nginx config settings: 

<pre>server {

    # listen, root, other top level config...

    # Reverse proxy for Bokeh server running on port 5006:

    location /bokeh {
        proxy_pass http://104.200.25.78:5006/bokeh;
    }

    location /static {
        proxy_pass http://104.200.25.78:5006;
    }

    location /bokehjs {
        proxy_pass http://104.200.25.78:5006;
    }

    # rest of the config...

}
</pre>

In terms of coding, the Bokeh model is a bit different to the usual plotting procedure in that you set up  
data sources, e.g. 

<pre>obj.line_source  = ColumnDataSource(data=dict(
                                            x_cV=[],
                                            arrival_date=[],
                                            laying_date=[],
                                            hatching_date=[],))
</pre>

and then plot commands use that data source. You don't pass NumPy arrays in directly: 

<pre>plot = figure(plot_height=400, plot_width=400,
              tools=toolset, x_range=[130, 180], y_range=[110, 180])

plot.line('x_cV', 'x_cV',          source=obj.line_source, line_width=4, color='black')
plot.line('x_cV', 'arrival_date',  source=obj.line_source, line_width=4, color='purple', legend='Arrival time')
plot.line('x_cV', 'laying_date',   source=obj.line_source, line_width=4, color='red',    legend='Laying time')
plot.line('x_cV', 'hatching_date', source=obj.line_source, line_width=4, color='green',  legend='Hatching date')
</pre>

Then, the input\_change method calls my update\_data method which actually updates the data sources. It doesn't have to explicitly make a call to redraw the plot. 

<pre>def update_data(self):
    u_q = self.u_q_slider.value

    self.line_source.data  = get_line_data_for_bokeh(float(u_q))
</pre>

## Links 

<https://github.com/carlohamalainen/phenology-two-trait-migratory-bird/tree/bokeh-slider> 

[http://bokeh.pydata.org/en/latest/docs/server\_gallery/sliders\_server.html](http://bokeh.pydata.org/en/latest/docs/server_gallery/sliders_server.html) 

[https://www.reddit.com/r/IPython/comments/3bgg7t/ipython\_widgets\_in\_a\_static\_html\_file](https://www.reddit.com/r/IPython/comments/3bgg7t/ipython_widgets_in_a_static_html_file) 

<https://github.com/ipython/ipywidgets/issues/16> 

<http://stackoverflow.com/questions/22739592/how-to-embed-an-interactive-matplotlib-plot-in-a-webpage> 

<https://jakevdp.github.io/blog/2013/12/05/static-interactive-widgets> 

## Update 7 January 2016 

The slider described above doesn't work with the latest version of Bokeh, so here's one that does: 

[bokehslider2017.py](https://github.com/carlohamalainen/phenology-two-trait-migratory-bird/blob/bokeh-slider/bokehslider2017.py) 

Launching is slightly different (see [this script](https://github.com/carlohamalainen/phenology-two-trait-migratory-bird/blob/bokeh-slider/run_bokeh_server-2017.sh)): 

<pre>bokeh serve bokehslider2017.py --host=carlo-hamalainen.net:443 --prefix=bokehslider 
                               --use-xheaders --port=5100 --allow-websocket-origin=carlo-hamalainen.net
</pre>

Happily for me this now plays nicely with my https nginx config: 

<pre>location /bokehslider {
            proxy_pass http://127.0.0.1:5100;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            proxy_http_version 1.1;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header Host $host:$server_port;
            proxy_buffering off;
        }
</pre>

And there is no need to open up port 5006 as I said before.