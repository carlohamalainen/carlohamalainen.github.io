---
title: Complex assignment in R
date: 2020-06-14T18:25:00+00:00
author: Carlo Hamalainen
layout: post
permalink: /2020/06/14/complex-assignment-in-R/
---

(Disclaimer: I'm not recommending the use of R, this question just
came up in some of [Nadiah's](https://nadiah.org) work. I suggest [Seaborn](https://seaborn.pydata.org/examples/index.html).)

If ``x`` is a dataframe in R, then ``colnames(x)`` gives you the column names:

    > colnames(x)
    [1] "Column1" "Column2" "Column3"

R supports "complex assignment", so you can do this:

    > colnames(x) <- c('a', 'b', 'c')
    > colnames(x)
    [1] "a" "b" "c"

and even sub-assignment:

    > colnames(x)[1] <- 'uhh'
    > colnames(x)
    [1] "uhh" "b"   "c"

As a Haskell developer this looks quite strange because you normally bind to
a name, not an expression, and especially not a sub-expression like ``colnames(x)[1]``.

I wondered if ``colnames(x)`` might be an object that overrides the bind somehow. That's how I might achieve
something similar in Python, although there is no ``__assign__`` to override as far as I am aware.

Looking in the R source code we find ``colnames`` in ``src/library/base/R/matrix.R``:

    $ git grep ^colnames | grep -v tests
    share/dictionaries/en_stats.txt:colnames
    src/library/base/R/matrix.R:colnames <- function(x, do.NULL = TRUE, prefix = "col")
    src/library/base/man/chol.Rd:colnames(x) <- letters[20:22]
    src/library/base/man/colnames.Rd:colnames(x, do.NULL = TRUE, prefix = "col")
    src/library/base/man/colnames.Rd:colnames(x) <- value
    src/library/base/man/colnames.Rd:colnames(m2, do.NULL = FALSE)
    src/library/base/man/colnames.Rd:colnames(m2) <- c("x","Y")
    src/library/base/man/dimnames.Rd:colnames0 <- function(x) dimnames(x)[[2]]
    src/library/base/man/isSymmetric.Rd:colnames(D3) <- c("X", "Y", "Z")
    src/library/datasets/data/EuStockMarkets.R:colnames(EuStockMarkets) <- c("DAX", "SMI", "CAC", "FTSE")
    src/library/grDevices/man/col2rgb.Rd:colnames(crgb) <- cc
    src/library/stats/man/cor.Rd:colnames(swM) <- abbreviate(colnames(swiss), min=6)
    src/library/stats/man/kmeans.Rd:colnames(x) <- c("x", "y")
    src/library/stats/man/printCoefmat.Rd:colnames(cmat) <- c("Estimate", "Std.Err", "Z value", "Pr(>z)")
    src/library/tools/R/sotools.R:colnames(so_symbol_names_table) <-
    src/library/tools/man/CRANtools.Rd:colnames(pdb)

After the definition of ``colnames`` there is an oddly named ``colnames<-``. At first sight this looks like a bit of a troll, creating a function
with bind in its name:

{% highlight r %}
colnames <- function(x, do.NULL = TRUE, prefix = "col")
{
    if(is.data.frame(x) && do.NULL)
    return(names(x))
    dn <- dimnames(x)
    if(!is.null(dn[[2L]]))
    dn[[2L]]
    else {
        nc <- NCOL(x)
    if(do.NULL) NULL
        else if(nc > 0L) paste0(prefix, seq_len(nc))
        else character()
    }
}

`colnames<-` <- function(x, value)
{
    if(is.data.frame(x)) {
        names(x) <- value
    } else {
        dn <- dimnames(x)
        if(is.null(dn)) {
            if(is.null(value)) return(x)
            if((nd <- length(dim(x))) < 2L)
                stop("attempt to set 'colnames' on an object with less than two dimensions")
            dn <- vector("list", nd)
        }
        if(length(dn) < 2L)
            stop("attempt to set 'colnames' on an object with less than two dimensions")
        if(is.null(value)) dn[2L] <- list(NULL) else dn[[2L]] <- value
        dimnames(x) <- dn
    }
    x
}
{% endhighlight %}

When we see

{% highlight r %}
foo(x) <- y
{% endhighlight %}

it can be expanded as

{% highlight r %}
`foo<-`(x, y)
{% endhighlight %}


It turns out there are three functions involved in evaluating an expression like

    colnames(x)[1] <- 'uhh'

Apart from ``colnames`` and ``colnames<-``, there is also a complex assignment for list-like things called ``[<-``.

Let's make our own versions of each and add some debug output:

{% highlight r %}
`colnames2<-` <- function(x, value)
{
    cat("colnames2<- ::: x\n")
    print(x)
    cat("\n")
    cat("colnames2<- ::: value\n")
    print(value)
    cat("\n")

    names(x) <- value
    x
}

colnames2 <- function(x)
{
    cat("colnames2 ::: x\n")
    print(x)
    cat("\n")

    return(colnames(x))
}

`[<-` <- function(x, idx, value)
{
    cat("square-bracket bind ::: x\n")
    print(x)
    cat("\n")
    cat("square-bracket bind ::: idx\n")
    print(idx)
    cat("\n")
    cat("square-bracket bind ::: value\n")
    print(value)
    cat("\n")

    x[[idx]] <- value
    x
}
{% endhighlight %}

Here is a test script:

{% highlight r %}
df <- data.frame(Column1=character(),
                 Column2=character(),
                 Column3=character(),
                 stringsAsFactors=FALSE)

colnames2(df)[1] <- c('uhh')
{% endhighlight %}

And this is the output:

    $ Rscript stupid.R 
    colnames2 ::: x
    [1] Column1 Column2 Column3
    <0 rows> (or 0-length row.names)

    square-bracket bind ::: x
    [1] "Column1" "Column2" "Column3"

    square-bracket bind ::: idx
    [1] 1

    square-bracket bind ::: value
    [1] "uhh"

    colnames2<- ::: x
    [1] Column1 Column2 Column3
    <0 rows> (or 0-length row.names)

    colnames2<- ::: value
    [1] "uhh"     "Column2" "Column3"



    Final value:

    [1] uhh     Column2 Column3
    <0 rows> (or 0-length row.names)

First, the column names:

    colnames2(x)

Next, list sub-assignment:

    ``[<-``(colnames2(x), 1, "uhh")

and

    ``colnames2<-``(x, ["uhh", "Column2", "Column3"])

In full, the expression ``colnames2(x)[1] <- 'uhh'`` is equivalent to

    x <- ``colnames2<-``(x, ``[<-``(colnames2(x), 1, 'uhh'))

Note that the return value of ``colnames2<-`` is the input dataframe/matrix.

Further reading:

* [https://github.com/wch/r-source/blob/trunk/src/library/base/R/matrix.R](https://github.com/wch/r-source/blob/trunk/src/library/base/R/matrix.R)
* [https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Subset-assignment](https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Subset-assignment)
* [https://stackoverflow.com/questions/45631545/rename-a-component-of-r-lists](https://stackoverflow.com/questions/45631545/rename-a-component-of-r-lists)

