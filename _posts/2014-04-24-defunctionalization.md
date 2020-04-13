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

Why are programs written in Java so verbose? The
answer seems to be that Java does not support function pointers or
functions as first class values. 

For example, solving an ODE in Python proceeds as follows. You define a
function for the derivative, and then pass that to the solver along
with some initial conditions and other parameters. Easy enough. 

{% highlight python %}
def yDot(y, c=[1.0, 1.0], omega=0.1):
    return [omega * (c[1] - y[1]), omega * (y[0] - c[0])]

finalValue = ode_solver(yDot, y0=[0.0, 1.0], t_final=16.0)
{% endhighlight %}

The equivalent in Java (example taken from <a href="http://commons.apache.org/proper/commons-math/userguide/ode.html">the Apache commons maths library</a>) requires an entire class to be written, implementing the oddly named interface ``FirstOrderDifferentialEquations``. 

```
private static class CircleODE implements FirstOrderDifferentialEquations {

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
```

This is a lot of work and obfuscates the problem at hand,
the definition of a derivative and a call to a solver. Java seems
to be unique in its pedantic style, at least among common languages in
use today. In C one can use function pointers, Fortran
can pass functions, Python has first-class functions, Haskell has
higher-order functions, and so on. 

It turns out that Java programmers are forced
to do <i>defunctionalization</i> since Java does not
support higher order functions. Here's a quote from <a href="view-source:http://blog.plover.com/prog/defunctionalization.html">a blog post on plover.net</a>:

> Defunctionalization is a program transformation that removes the
higher-order functions from a program.  The idea is that you replace
something like &lambda;<i>x</i>.<i>x</i>+<i>y</i>  with a data structure that
encapsulates a value of <i>y</i> somewhere, say (HOLD <i>y</i>).  And
instead of using the language's built-in function application to
apply this object directly to an argument <i>x</i>, you write a
synthetic applicator that takes (HOLD <i>y</i>) and <i>x</i> and
returns <i>x</i> + <i>y</i>.     And anyone who wanted to apply
&lambda;<i>x</i>.<i>x</i>+<i>y</i> to some argument <i>x</i> in some context
in which <i>y</i> was bound should first construct (HOLD <i>y</i>),
then use the synthetic applicator on (HOLD <i>y</i>) and <i>x</i>.  </blockquote> 

In Haskell we might implement ``yDot`` as: 

{% highlight haskell %}
module Defun where

yDot :: [Double] -> Double -> [Double] -> [Double]
yDot c omega y = [omega * (c !! 1 - y !! 1), omega * (y !! 0 - c !! 0)]
{% endhighlight %}

The parameters ``c`` and ``omega`` are the slowest varying, so we put them before ``y``. Since all functions in Haskell are <a href="http://www.haskell.org/haskellwiki/Currying">curried</a>, we can conveniently produce the function that we need by partially applying the ``c`` and ``omega`` values:

```
*Defun> :t yDot [1.0, 1.0] 0.1
yDot [1.0, 1.0] 0.1 :: [Double] -> [Double]
```

In this way ``yDot`` is a higher order function. To make it first-order we have to defunctionalize it. Following the example on <a href="view-source:http://blog.plover.com/prog/defunctionalization.html">plover.net</a> we define a data structure to hole the ``c`` and ``omega`` values: 

{% highlight haskell %}
data Hold = MkHold [Double] Double
{% endhighlight %}

And we need a function to "apply" this value to get the actual ``yDot`` value. 

{% highlight haskell %}
fakeApply :: Hold -> [Double] -> [Double]
fakeApply (MkHold c omega) y = [omega * (c !! 1 - y !! 1), omega * (y !! 0 - c !! 0)]
{% endhighlight %}

Basically ``Hold`` and ``fakeApply`` are equivalent to the ``CircleODE`` class above. 

Example: 

{% highlight haskell %}
hold :: Hold
hold = MkHold [1.0, 1.0] 0.1

result :: [Double]
result = fakeApply hold [1.0, 1.0]
{% endhighlight %}

Defunctionalization appears to be the cause of the excessive use of nouns in Java code, resulting in things like the <a href="http://docs.spring.io/spring/docs/2.5.x/api/org/springframework/aop/framework/AbstractSingletonProxyFactoryBean.html">Abstract Singleton Proxy Factory Bean</a>, or the <a href="http://en.wikipedia.org/wiki/Abstract_factory_pattern">Abstract Factory</a> design pattern.

Further reading: 

* Defunctionalization and Java: <a href="http://blog.plover.com/prog/defunctionalization.html">http://blog.plover.com/prog/defunctionalization.html</a> (<a href="/stuff/defunctionalization_local_copies/The%20Universe%20of%20Discourse%20%20%20Defunctionalization%20and%20Java.html">local copy</a>)

* Ken Knowles' blog post: <a href="https://github.com/kennknowles/functional-lens/blob/master/2008/2008-05-24-Defunctionalization/Defunctionalization.lhs">https://github.com/kennknowles/functional-lens/blob/master/2008/2008-05-24-Defunctionalization/Defunctionalization.lhs</a> (<a href="/stuff/defunctionalization_local_copies/Defunctionalization.lhs">local copy</a>)

* Steve Yegge's rant on execution in the kingdom of nouns: <a href="http://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html">http://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html</a> (<a href="/stuff/defunctionalization_local_copies/Stevey%27s%20Blog%20Rants%20%20Execution%20in%20the%20Kingdom%20of%20Nouns.html">local copy</a>)

Literate Haskell source for this post is here: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/java-defun">https://github.com/carlohamalainen/playground/tree/master/haskell/java-defun</a>. 

<p><center> <a href="http://www.howtogeek.com/123403/the-world-as-seen-by-an-object-oriented-programmer-comic/"><img src="/stuff/world_according_to_OO_programmer.jpg"></a> </center> 
