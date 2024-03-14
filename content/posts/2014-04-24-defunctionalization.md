---
author: Carlo Hamalainen

date: "2014-04-24T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2014/04/24/defunctionalization/
id: 833
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: Defunctionalization
url: /2014/04/24/defunctionalization/
---

Why are programs written in Java so verbose? The
answer seems to be that Java does not support function pointers or
functions as first class values. 

For example, solving an ODE in Python proceeds as follows. You define a
function for the derivative, and then pass that to the solver along
with some initial conditions and other parameters. Easy enough. 

```python
def yDot(y, c=[1.0, 1.0], omega=0.1):
    return [omega * (c[1] - y[1]), omega * (y[0] - c[0])]

finalValue = ode_solver(yDot, y0=[0.0, 1.0], t_final=16.0)
```

The equivalent in Java (example taken from [the Apache commons maths library](http://commons.apache.org/proper/commons-math/userguide/ode.html)) requires an entire class to be written, implementing the oddly named interface ``FirstOrderDifferentialEquations``. 

```java
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
to do *defunctionalization* since Java does not
support higher order functions. Here's a quote from [a blog post on plover.net](view-source:http://blog.plover.com/prog/defunctionalization.html):

> Defunctionalization is a program transformation that removes the
higher-order functions from a program.  The idea is that you replace
something like &lambda;*x*.*x*+*y*  with a data structure that
encapsulates a value of *y* somewhere, say (HOLD *y*).  And
instead of using the language's built-in function application to
apply this object directly to an argument *x*, you write a
synthetic applicator that takes (HOLD *y*) and *x* and
returns *x* + *y*.     And anyone who wanted to apply
&lambda;*x*.*x*+*y* to some argument *x* in some context
in which *y* was bound should first construct (HOLD *y*),
then use the synthetic applicator on (HOLD *y*) and *x*.

In Haskell we might implement ``yDot`` as: 

```haskell
module Defun where

yDot :: [Double] -> Double -> [Double] -> [Double]
yDot c omega y = [omega * (c !! 1 - y !! 1), omega * (y !! 0 - c !! 0)]
```

The parameters ``c`` and ``omega`` are the slowest varying, so we put them before ``y``. Since all functions in Haskell are [curried](http://www.haskell.org/haskellwiki/Currying), we can conveniently produce the function that we need by partially applying the ``c`` and ``omega`` values:

```
*Defun> :t yDot [1.0, 1.0] 0.1
yDot [1.0, 1.0] 0.1 :: [Double] -> [Double]
```

In this way ``yDot`` is a higher order function. To make it first-order we have to defunctionalize it. Following the example on [plover.net](http://blog.plover.com/prog/defunctionalization.html) we define a data structure to hole the ``c`` and ``omega`` values: 

```haskell
data Hold = MkHold [Double] Double
```

And we need a function to "apply" this value to get the actual ``yDot`` value. 

```haskell
fakeApply :: Hold -> [Double] -> [Double]
fakeApply (MkHold c omega) y = [omega * (c !! 1 - y !! 1), omega * (y !! 0 - c !! 0)]
```

Basically ``Hold`` and ``fakeApply`` are equivalent to the ``CircleODE`` class above. 

Example: 

```haskell
hold :: Hold
hold = MkHold [1.0, 1.0] 0.1

result :: [Double]
result = fakeApply hold [1.0, 1.0]
```

Defunctionalization appears to be the cause of the excessive use of nouns in Java code, resulting in things like the [Abstract Singleton Proxy Factory Bean](http://docs.spring.io/spring/docs/2.5.x/api/org/springframework/aop/framework/AbstractSingletonProxyFactoryBean.html), or the [Abstract Factory](http://en.wikipedia.org/wiki/Abstract_factory_pattern) design pattern.

Further reading: 

Defunctionalization and Java: [http://blog.plover.com/prog/defunctionalization.html](http://blog.plover.com/prog/defunctionalization.html) 
([local copy](/stuff/defunctionalization_local_copies/The%20Universe%20of%20Discourse%20%20%20Defunctionalization%20and%20Java.html))

Ken Knowles' blog post: [https://github.com/kennknowles/functional-lens/blob/master/2008/2008-05-24-Defunctionalization/Defunctionalization.lhs](https://github.com/kennknowles/functional-lens/blob/master/2008/2008-05-24-Defunctionalization/Defunctionalization.lhs) 
([local copy](/stuff/defunctionalization_local_copies/Defunctionalization.lhs))

Steve Yegge's rant on execution in the kingdom of nouns: [http://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html](http://steve-yegge.blogspot.com/2006/03/execution-in-kingdom-of-nouns.html) ([local copy](/stuff/defunctionalization_local_copies/Stevey%27s%20Blog%20Rants%20%20Execution%20in%20the%20Kingdom%20of%20Nouns.html))

Literate Haskell source for this post is here: [https://github.com/carlohamalainen/playground/tree/master/haskell/java-defun](https://github.com/carlohamalainen/playground/tree/master/haskell/java-defun). 

{{< figure src="/stuff/world_according_to_OO_programmer.jpg" link="http://www.howtogeek.com/123403/the-world-as-seen-by-an-object-oriented-programmer-comic/" >}}

