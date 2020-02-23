---
id: 684
title: A few dissections of order 18
date: 2012-03-17T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2012/03/17/a-few-dissections-of-order-18/
permalink: /2012/03/17/a-few-dissections-of-order-18/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
A _triangle dissection_ of an equilateral triangle is a way of dividing up a original triangle into smaller equilateral triangles, such that none of the smaller triangles overlap (for a few examples, scroll down in this post). Due to a [known link](http://arxiv.org/abs/0910.5199) between spherical latin bitrades and triangle dissections, we are able to enumerate triangle dissections exhaustively up to a certain size. 

One way to calculate a _canonical signature_ for each dissection is to take the list of triangles, the vertices of which live in<img src="//s0.wp.com/latex.php?latex=mathbb%7BQ%7D%28sqrt%7B3%7D%29+times+mathbb%7BQ%7D%28sqrt%7B3%7D%29&#038;bg=ffffff&#038;fg=000000&#038;s=0" alt="mathbb{Q}(sqrt{3}) times mathbb{Q}(sqrt{3})" title="mathbb{Q}(sqrt{3}) times mathbb{Q}(sqrt{3})" class="latex" /> , and take the sorted minimum sequence of sorted triples. We can write down a convenient representation (avoiding irrational numbers) by noting that elements of<img src="//s0.wp.com/latex.php?latex=mathbb%7BQ%7D%28sqrt%7B3%7D%29&#038;bg=ffffff&#038;fg=000000&#038;s=0" alt="mathbb{Q}(sqrt{3})" title="mathbb{Q}(sqrt{3})" class="latex" /> have the form<img src="//s0.wp.com/latex.php?latex=a+%2B+bmathbb%7BQ%7D%28sqrt%7B3%7D%29&#038;bg=ffffff&#038;fg=000000&#038;s=0" alt="a + bmathbb{Q}(sqrt{3})" title="a + bmathbb{Q}(sqrt{3})" class="latex" /> where<img src="//s0.wp.com/latex.php?latex=a%2C%2Cb+in+mathbb%7BQ%7D&#038;bg=ffffff&#038;fg=000000&#038;s=0" alt="a,,b in mathbb{Q}" title="a,,b in mathbb{Q}" class="latex" /> . For example, the first dissection shown below has the following signature:

<pre>0 0 0 0 1/1 2 0 0 1/12 1/6 0 0 0 1/12 0 0 1/12 1/6 0 0 0 1/4 0 0 1/12 1/12 0 0 1/12
1/6 0 0 1/6 1/4 0 0 1/12 1/6 0 0 0 1/4 0 0 1/12 1/3 0 0 0 1/6 0 0 1/6 1/4 0 0 1/12
1/3 0 0 1/6 1/6 0 0 1/6 1/4 0 0 1/4 1/3 0 0 1/6 1/4 0 0 1/12 1/3 0 0 0 5/12 0 0 1/12
1/4 0 0 1/12 5/12 0 0 1/4 7/12 0 0 1/12 1/4 0 0 1/4 1/3 0 0 1/6 5/12 0 0 1/4 1/4 0
0 1/4 1/3 0 0 1/3 5/12 0 0 1/4 1/3 0 0 0 5/12 0 0 1/12 1/2 0 0 0 1/3 0 0 1/3 1/2 0
0 1/6 2/3 0 0 1/3 1/3 0 0 1/3 1/2 0 0 1/2 2/3 0 0 1/3 5/12 0 0 1/12 1/2 0 0 0 7/12 0
0 1/12 1/2 0 0 0 7/12 0 0 1/12 2/3 0 0 0 1/2 0 0 1/6 2/3 0 0 0 5/6 0 0 1/6 1/2 0 0
1/6 2/3 0 0 1/3 5/6 0 0 1/6 2/3 0 0 0 5/6 0 0 1/6 1 0 0 0
</pre>

(This signature can be converted to a PDF by echoing it as a single line to the Python script [draw_dissection.py](https://github.com/carlohamalainen/triangle_dissections/blob/master/plot/draw_dissection.py)) 

While double-checking my C++ implementation I found that my old Python code was finding 5 fewer dissections of order 18, when counting both separated and nonseparated dissections (separated dissections have interior vertices of degree 4, while nonseparated dissections have interior vertices of degree 4 or 6). It turned out to be the way that I was calculating a canonical signature. In the interest of saving space, my old Python code just stored a list of vertices, so naturally it lost information about whether a vertex was of degree 4 or 6. Surprisingly this caused no problems until order 18. The 5 pairs of vertex-equivalent dissections are shown below:

[<img src="https://i0.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_0.png?w=1100&#038;ssl=1" data-recalc-dims="1" />     ](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_0.pdf) [<img src="https://i1.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_1.png?w=1100&#038;ssl=1" data-recalc-dims="1" />](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_1.pdf)

[<img src="https://i2.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_2.png?w=1100&#038;ssl=1" data-recalc-dims="1" />     ](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_2.pdf) [<img src="https://i2.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_3.png?w=1100&#038;ssl=1" data-recalc-dims="1" />](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_3.pdf)

[<img src="https://i0.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_4.png?w=1100&#038;ssl=1" data-recalc-dims="1" />     ](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_4.pdf) [<img src="https://i0.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_5.png?w=1100&#038;ssl=1" data-recalc-dims="1" />](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_5.pdf)

[<img src="https://i0.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_6.png?w=1100&#038;ssl=1" data-recalc-dims="1" />     ](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_6.pdf) [<img src="https://i2.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_7.png?w=1100&#038;ssl=1" data-recalc-dims="1" />](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_7.pdf)

[<img src="https://i0.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_8.png?w=1100&#038;ssl=1" data-recalc-dims="1" />     ](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_8.pdf) [<img src="https://i1.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_9.png?w=1100&#038;ssl=1" data-recalc-dims="1" />](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/dissections_18/n18_sig_9.pdf)