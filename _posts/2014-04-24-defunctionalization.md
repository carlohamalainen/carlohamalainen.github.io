---
id: 833
title: Defunctionalization
date: 2014-04-24T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/04/24/defunctionalization/
permalink: /2014/04/24/defunctionalization/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Why are programs written in Java so verbose? The answer seems to be that Java does not support function pointers or functions as first class values. 

For example, solving an ODE in Python proceeds as follows. You define a function for the derivative, and then pass that to the solver along with some initial conditions and other parameters. Easy enough.

<pre>def yDot(y, c=[1.0, 1.0], omega=0.1):
    return [omega * (c[1] - y[1]), omega * (y[0] - c[0])]

finalValue = ode_solver(yDot, y0=[0.0, 1.0], t_final=16.0)
</pre>

The equivalent in Java (example taken from [the Apache commons maths library](http://commons.apache.org/proper/commons-math/userguide/ode.html)) requires an entire class to be written, implementing the oddly named interface FirstOrderDifferentialEquations. 

<pre>private static class CircleODE implements FirstOrderDifferentialEquations {

    private double[] c;
    private double omega;

    public CircleODE(double[] c, double omega) {
        this.c     = c;
        this.omega = omega;
    }

    public int getDimension() {
        return 2;
    }

    public void computeDerivatives(double t, double[] y, double[] yDot) {
        yDot[0] = omega * (c[1] - y[1]);
        yDot[1] = omega * (y[0] - c[0]);
    }

}

// then in some other class...

FirstOrderIntegrator dp853 = new DormandPrince853Integrator(1.0e-8, 100.0, 1.0e-10, 1.0e-10);
FirstOrderDifferentialEquations ode = new CircleODE(new double[] { 1.0, 1.0 }, 0.1);
double[] y = new double[] { 0.0, 1.0 }; // initial state
dp853.integrate(ode, 0.0, y, 16.0, y); // now y contains final state at time t=16.0
</pre>

This is a lot of work and obfuscates the problem at hand, the definition of a derivative and a call to a solver. Java seems to be unique in its pedantic style, at least among common languages in use today. In C one can use function pointers, Fortran can pass functions, Python has first-class functions, Haskell has higher-order functions, and so on.

It turns out that Java programmers are forced to do _defunctionalization_ since Java does not support higher order functions. Here’s a quote from [a blog post on plover.net](http://blog.plover.com/prog/defunctionalization.html):

> Defunctionalization is a program transformation that removes the higher-order functions from a program. The idea is that you replace something like λ_x_._x_+_y_ with a data structure that encapsulates a value of _y_ somewhere, say (HOLD _y_). And instead of using the language’s built-in function application to apply this object directly to an argument _x_, you write a synthetic applicator that takes (HOLD _y_) and _x_ and returns _x_ + _y_. And anyone who wanted to apply λ_x_._x_+_y_ to some argument _x_ in some context in which _y_ was bound should first construct (HOLD _y_), then use the synthetic applicator on (HOLD _y_) and _x_.

In Haskell we might implement yDot as follows: 

<pre>&gt; module Defun where
&gt;
&gt; yDot :: [Double] -&gt; Double -&gt; [Double] -&gt; [Double]
&gt; yDot c omega y = [omega * (c !! 1 - y !! 1), omega * (y !! 0 - c !! 0)]
</pre>

The parameters c and omega are the slowest varying, so we put them before y. Since all functions in Haskell are [curried](http://www.haskell.org/haskellwiki/Currying), we can conveniently produce the function that we need by partially applying the c and omega values:

<pre>*Defun&gt; :t yDot [1.0, 1.0] 0.1
yDot [1.0, 1.0] 0.1 :: [Double] -&gt; [Double]
</pre>

In this way yDot is a higher order function. To make it first-order we have to defunctionalize it. Following the example on [plover.net](http://blog.plover.com/prog/defunctionalization.html) we define a data structure to hole the c and omega values:

<pre>&gt; data Hold = MkHold [Double] Double
</pre>

And we need a function to “apply” this value to get the actual yDot value.

<pre>&gt; fakeApply :: Hold -&gt; [Double] -&gt; [Double]
&gt; fakeApply (MkHold c omega) y = [omega * (c !! 1 - y !! 1), omega * (y !! 0 - c !! 0)]
</pre>

Basically Hold and fakeApply are equivalent to the CircleODE class above. 

Example: 

<pre>&gt; hold :: Hold
&gt; hold = MkHold [1.0, 1.0] 0.1
&gt;
&gt; result :: [Double]
&gt; result = fakeApply hold [1.0, 1.0]
</pre>

Defunctionalization appears to be the cause of the excessive use of nouns in Java code, resulting in things like the [Abstract Singleton Proxy Factory Bean](http://docs.spring.io/spring/docs/2.5.x/api/org/springframework/aop/framework/AbstractSingletonProxyFactoryBean.html), or the [Abstract Factory](http://en.wikipedia.org/wiki/Abstract_factory_pattern) design pattern.

Further reading: 

  * Defunctionalization and Java: <http://blog.plover.com/prog/defunctionalization.html> ([local copy](http://carlo-hamalainen.net/stuff/defunctionalization_local_copies/The%20Universe%20of%20Discourse%20%20%20Defunctionalization%20and%20Java.html)) 
  * Ken Knowles&#8217; blog post: <https://github.com/kennknowles/functional-lens/blob/master/2008/2008-05-24-Defunctionalization/Defunctionalization.lhs> ([local copy](http://carlo-hamalainen.net/stuff/defunctionalization_local_copies/Defunctionalization.lhs)) 
  * Steve Yegge&#8217;s rant on execution in the kingdom of nouns: <http://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html> ([local copy](http://carlo-hamalainen.net/stuff/defunctionalization_local_copies/Stevey%27s%20Blog%20Rants%20%20Execution%20in%20the%20Kingdom%20of%20Nouns.html)) 

Literate Haskell source for this post is here: <https://github.com/carlohamalainen/playground/tree/master/haskell/java-defun>. 



[<img src="https://i1.wp.com/s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/world_according_to_OO_programmer.jpg?w=1100&#038;ssl=1" data-recalc-dims="1" />](http://www.howtogeek.com/123403/the-world-as-seen-by-an-object-oriented-programmer-comic/)