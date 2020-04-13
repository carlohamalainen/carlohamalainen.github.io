---
title: Honesty in science
date: 2009-08-22T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2009/08/22/honesty-in-science/
permalink: /2009/08/22/honesty-in-science/
---

In my day job I work on mathematics (discrete mathematics, mainly to do with latin squares). My research output is modest. It is by no means ground-breaking stuff. I doubt that I'll ever win the Clay maths prize for solving the Riemann hypothesis, nor will I ever win the Fields medal.

I can, however, take credit that I have always focussed on the mathematics itself. I've stated my claims as clearly as possible, provided source code to support computational claims, and made as much of my work freely available on reliable sites (mainly on the [arXiv](http://arxiv.org)). What you see is what you get. On a daily basis I throw away a lot of what I do. That's the nature of mathematics and science.

It then saddens me to read things like this: [Dembski does it again](http://scienceblogs.com/pharyngula/2009/08/dembski_does_it_again.php). The paper in question is pushing another agenda through an otherwise normal scientific journal. This is an offence to the journal and the other people who have published there.

The paper itself is surreal. On p. 1054:

> The simple idea of importance sampling is to query more frequently near to the target. As shown in Fig. 1, active information is introduced by variation of the distribution of the search space to one where more probability mass is concentrated about the target.
> 
> Our use of importance sampling is not conventional. The procedure is typically used to determine expected values using Monte Carlo simulation using fewer randomly generated queries by focusing attention on queries closer to the target. We are interested, rather, in locating a single point in T.

The term "importance sampling" is very well known in the statistics community. It has a very clear and agreed-upon meaning. To change that is silly at best, and dishonest at worst. In fact, misusing well known terms is a good sign that you're a [crank](http://en.wikipedia.org/wiki/Crank_(person)).

One standard application of importance sampling is to estimate an integral numerically. For a simple example, you might want to estimate the area under the curve in this picture:

<img src="/stuff/curve_for_integral.png?w=1100" /> 

The main benefit of importance sampling is that your estimates will be "close together" (mathematically, their standard deviation will be smaller than a uniform sampling Monte Carlo approach). The red dots in the following plot are the absolute errors from a uniform sampling estimator of the value of the integral, while the green dots are the absolute errors from an importance sampling estimator:

<img src="https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/integral_errors_g.png?w=1100&ssl=1" data-recalc-dims="1" /> 

More precisely, the standard deviation of the errors of the uniform and importance sampling estimates is 0.0193 and 0.0001, respectively. The code is available as a [Sage](http://sagemath.org) worksheet: [Importance_sampling.sws](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/Importance_sampling.sws).

For some reason, Dembski says that "We are interested, rather, in locating a single point in T". In our picture, the only sensible thing that his statement could mean is that he wants to find the peak of the curve. Right? But we have algorithms specifically formulated for doing that sort of thing. Back in undergrad I remember learning about [Newton's method](http://en.wikipedia.org/wiki/Newton%27s_method#Application_to_minimization_and_maximization_problems) and other search/optimisation algorithms. 

If you're searching for a point in some space, then just say that.

For a thorough rebuke of the information theory rubbish in Dembski's paper, see [Mark C. Chu-Carroll's post](http://scienceblogs.com/goodmath/2009/05/_so_william_dembski_the.php).
