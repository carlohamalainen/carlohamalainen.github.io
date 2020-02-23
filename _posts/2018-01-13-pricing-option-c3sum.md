---
id: 1081
title: Pricing an option on a cumulative sum.
date: 2018-01-13T01:37:39+00:00
author: Carlo Hamalainen
layout: post
guid: https://carlo-hamalainen.net/?p=1081
permalink: /2018/01/13/pricing-option-c3sum/
categories:
  - Uncategorized
---
Consider an [option](https://en.wikipedia.org/wiki/Option_(finance)) with payout on an underlying which is a cumulative sum. For example, a construction company might want to mitigate the risk of rainfall causing delays in a project. The construction company could buy a call option on the cumulative sum of rainfall not exceeding 180mm on any 3-day period. Further, we restrict this so that one day can only be used in one payoff. If the cumulative rainfall $r$ a 3-day period exceeded 180mm, the entity selling the call option would have the obligation to pay the construction company based on the difference $r &#8211; 180$.

As the entity selling one of these options, we need to work out the expected loss and worst possible scenario for payouts.

Let $r\_0, r\_1, \ldots$ be the sequence of daily rainfall. Form the 3-sums:

\[  
s\_0 = r\_0+r\_1+r\_2, s\_1 = r\_1+r\_2+r\_3, s\_2 = r\_2+r\_3+r\_4, \ldots  
\]

Let $C > 0$ be the cutoff (e.g. 180mm/day). Then form the sequence $w\_0, w\_1, \ldots$ where

\[  
w\_i = f\_C(s_i)  
\]

where $f_C(x)$ is $0$ if $x \leq C$ and $x$ otherwise.

As the seller of the option, the worst outcome for us (i.e. largest payout) corresponds to the subsequence

\[  
w\_{k\_1}, w\_{k\_2}, \ldots w\_{k\_n}  
\]

that maximises the sum $\sum\_{l=1}^n w\_{k\_l}$ for some $n$, where the terms $w\_{k_l}$ are mutually disjoint (i.e. we don&#8217;t take the 3-sum of any days that overlap). We consider this sum over one year, but the same approach applies to monthly, quarterly, etc.

### Formulation as a graph problem

Label vertices with the 3-sums $w\_i$ and create an edge $(w\_i, w\_j)$ if the sums $w\_i$ and $w_j$ refer to overlapping time periods. Then our problem is equivalent to finding a Maximum Weighted Independent Set in the graph. This is a classic [NP-hard](https://en.wikipedia.org/wiki/Independent_set_(graph_theory)#Finding_independent_sets) problem.

### Formulation as a linear programming problem

Create binary decision variables $x\_0, x\_1, \ldots$ and optimise

\[ \sum{w\_i x\_i} \]

subject to

\[  
x\_i + x\_j \leq 1  
\]

when $w\_i$ and $w\_j$ refer to non-disjoint cumulative 3-sums.

### Python solver

Using [PuLP](https://www.coin-or.org/PuLP/index.html) we can very quickly write a solver in Python.

First set up a solver:

<pre class="brush: python; title: ; notranslate" title="">prob = pulp.LpProblem('maxcsums', pulp.LpMaximize)
</pre>

Create binary variables corresponding to each vertex:

<pre class="brush: python; title: ; notranslate" title="">x_vars = [ pulp.LpVariable('x' + str(i), cat='Binary')
             for (i, _) in enumerate(weights) ]
</pre>

Use [reduce](https://docs.python.org/2/library/functions.html#reduce) to form the sum \( \sum w\_i x\_i \):

<pre class="brush: python; title: ; notranslate" title="">prob += reduce(lambda a,b: a+b,
                   [ weights[i]*x_vars[i] for i in range(len(weights))])
</pre>

Finally, given a list of indices (3-tuples), add the constraints to avoid solutions with adjacent vertices:

<pre class="brush: python; title: ; notranslate" title="">for (i, x) in enumerate(indices):
  for (j, y) in enumerate(indices):
    if x &lt; y:
      if is_adjacent(x, y):
        prob += x_vars[i] + x_vars[j] &lt;= 1
</pre>

### Changi daily rainfall

For a concrete example, let&#8217;s use the [daily rainfall data for Changi](http://www.weather.gov.sg/climate-historical-daily/) in Singapore.

Here are the 3-sums:

<center>
  <br /> <img class="alignnone size-medium wp-image-1098" src="https://i0.wp.com/carlo-hamalainen.net/wp-content/uploads/2018/01/c3sums.png?w=500&#038;ssl=1" alt="" data-recalc-dims="1" /><br />
</center>

For cutoffs from 100 to 300, here are the number of extreme events per year:

<pre class="brush: plain; title: ; notranslate" title="">100 120 140 160 180 200 220 240 260 280 300
1980   3   3   2   1   1   1   1   1   1   0   0
1981   1   1   1   1   0   0   0   0   0   0   0
1982   2   1   1   1   1   1   1   1   0   0   0
1983   2   2   2   2   2   1   0   0   0   0   0
1984   6   4   2   1   1   1   0   0   0   0   0
1985   1   1   0   0   0   0   0   0   0   0   0
1986   4   4   4   3   1   0   0   0   0   0   0
1987   4   3   2   2   1   1   1   1   1   0   0
1988   9   4   2   1   1   0   0   0   0   0   0
1989   3   1   1   1   1   1   1   1   0   0   0
1990   2   1   1   1   0   0   0   0   0   0   0
1991   4   3   1   0   0   0   0   0   0   0   0
1992   6   3   2   2   1   1   1   0   0   0   0
1993   1   0   0   0   0   0   0   0   0   0   0
1994   4   3   2   2   2   1   0   0   0   0   0
1995   3   3   3   2   2   2   1   0   0   0   0
1996   1   0   0   0   0   0   0   0   0   0   0
1997   0   0   0   0   0   0   0   0   0   0   0
1998   3   3   1   1   0   0   0   0   0   0   0
1999   2   0   0   0   0   0   0   0   0   0   0
2000   2   2   0   0   0   0   0   0   0   0   0
2001   5   3   2   2   2   2   2   2   2   1   0
2002   4   1   1   1   1   0   0   0   0   0   0
2003   5   1   1   1   1   1   0   0   0   0   0
2004   6   4   2   2   1   1   1   0   0   0   0
2005   2   1   1   1   0   0   0   0   0   0   0
2006   5   5   5   5   3   2   2   1   1   1   1
2007   7   5   4   3   1   1   1   1   1   0   0
2008   4   3   1   0   0   0   0   0   0   0   0
2009   1   1   0   0   0   0   0   0   0   0   0
2010   4   1   0   0   0   0   0   0   0   0   0
2011   4   2   1   1   1   1   1   1   1   1   0
2012   3   0   0   0   0   0   0   0   0   0   0
2013   5   3   3   1   0   0   0   0   0   0   0
2014   0   0   0   0   0   0   0   0   0   0   0
2015   0   0   0   0   0   0   0   0   0   0   0
2016   1   0   0   0   0   0   0   0   0   0   0
</pre>

We will go with 180mm as the cutoff. Ultimately, choosing the strike is a business decision.

The burn is $\texttt{max}(w_i &#8211; 180, 0)$, indicating the payout that the seller of the option would be liable for. Here is the daily burn for Changi daily rainfall (zero rows are not shown):

<pre class="brush: plain; title: ; notranslate" title="">c3sum   burn
DATE                    
1980-01-20  275.5   95.5
1982-12-22  245.4   65.4
1983-08-22  190.6   10.6
1983-12-26  218.8   38.8
1984-02-02  219.3   39.3
1986-12-11  186.7    6.7
1987-01-11  260.5   80.5
1988-09-21  188.9    8.9
1989-12-01  252.5   72.5
1992-11-11  223.2   43.2
1994-11-11    209   29.0
1994-12-19  180.7    0.7
1995-11-02  203.4   23.4
1995-02-06  236.9   56.9
2001-01-16  268.7   88.7
2001-12-27  281.4  101.4
2002-01-25  182.1    2.1
2003-01-31  210.5   30.5
2004-01-25  237.9   57.9
2006-12-19  356.4  176.4
2006-12-26  222.7   42.7
2006-01-09  190.7   10.7
2007-01-12  260.4   80.4
2011-01-31  298.2  118.2
</pre>

The yearly burn is $\sum\_i{\texttt{max}(w\_i-180,0)}$ where the $w_i$ are in a particular calendar year. Plotting the yearly burn:

<center>
  <br /> <img class="alignnone" src="https://i2.wp.com/raw.githubusercontent.com/carlohamalainen/maxc3sum/master/yearly_burn.png?w=1100&#038;ssl=1" alt="" data-recalc-dims="1" /><br />
</center>

The histogram of burn values shows that a value around $80$ is the most frequent total yearly burn:

<center>
  <br /> <img class="alignnone" src="https://i2.wp.com/raw.githubusercontent.com/carlohamalainen/maxc3sum/master/yearly_burn_histogram.png?w=500&#038;ssl=1" alt="" data-recalc-dims="1" /><br />
</center>

Cumulative histogram of the yearly burn: 

<center>
  <br /> <img class="alignnone" src="https://i1.wp.com/raw.githubusercontent.com/carlohamalainen/maxc3sum/master/yearly_burn_cumulative.png?w=500&#038;ssl=1" alt="" data-recalc-dims="1" /><br />
</center>

From the yearly burn we can compute the expected loss over various periods:

<pre class="brush: plain; title: ; notranslate" title="">Expected loss (all years):     71.13
Expected loss (last 10 years): 86.22
Expected loss (last 20 years): 71.13
</pre>

We see that the expected loss over the last 10, 20 years differs by about 15.0.  
The 99th percentile is p99 = 223.051.

Unlike pricing options on a liquid instrument such as an interest rate swap, stock, currencies, etc, the pricing of this option is set in an incomplete market. There is no way to hedge, so the Black Scholes formula cannot be used. Typically the price will be set as

\[  
f(\texttt{expected loss}, \texttt{high value on the tail})  
\]

where the high value on the tail is p99 or p99.9 (or similar); the function $f$ will integrate the price maker&#8217;s risk appetite, but it will be approximately

\[  
\texttt{option price} \approx \texttt{expected loss} + \Delta  
\]

### Notes 

Source code is on Github: <https://github.com/carlohamalainen/maxc3sum>

Most of the data manipulation is done using [Python Pandas](https://pandas.pydata.org/) which makes lots of things one-liners, e.g. to sum the daily burn to yearly burn, given an index of [datetime.date](https://docs.python.org/2/library/datetime.html), we can [group by](https://pandas.pydata.org/pandas-docs/stable/groupby.html) year and then [sum](https://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.sum.html) the result: 

<pre class="brush: python; title: ; notranslate" title="">yearly_burn = daily_burn.groupby(lambda x: x.year).sum()
</pre>

To set the [gap for GLPK](https://en.wikibooks.org/wiki/GLPK/Using_GLPSOL), pass the command line option when setting up the initial GLPK object:

<pre class="brush: python; title: ; notranslate" title="">pulp.GLPK(options=['--mipgap', 0.1]).solve(prob)
</pre>

Naturally, finding a good gap is problem-specific and requires some experimentation.