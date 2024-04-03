---
author: Carlo Hamalainen

date: "2011-08-04T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2011/08/04/ggplot2-from-clojure/
title: ggplot2 from Clojure
url: /2011/08/04/ggplot2-from-clojure/
---
Just a quick note on how to call ggplot2 from Clojure. Install the [rincanter](https://github.com/jolby/rincanter) package. A tip: if you don't know what ``R_HOME`` is for your system, try this at the R prompt:

    > R.home(component="home")
    [1] "/usr/lib64/R"
    >

So I did ``export R_HOME=/usr/lib64/R`` and then rincanter was happy.

When calling qplot, do not use r-eval because this tries to convert the entire plot into a Clojure object, resulting in unmanageable output. Use r-eval-raw instead.

<https://gist.github.com/1124157>

```clojure
; http://carlo-hamalainen.net/blog/2011/08/04/ggplot2-from-clojure

; To dump the plot to a file:
(use '(com.evocomputing rincanter)) ; https://github.com/jolby/rincanter
(r-eval "library(ggplot2)")
(r-eval-raw "qplot(rating, data=movies, geom=\"histogram\")") ; see http://had.co.nz/ggplot2/geom_histogram.html
(r-eval "ggsave('histogram-example.png')")

; To display on your screen (Unix example; see rincanter docs for alternatives to x11() call)
(use '(com.evocomputing rincanter))
(r-eval "x11()")
(r-eval "library(ggplot2)")
(r-eval-raw "p = qplot(rating, data=movies, geom=\"histogram\")")
(r-eval-raw "print(p)")
```

I worked out the right commands by copying one of the answers to this [stackoverflow question](http://stackoverflow.com/questions/3515269/ggplot2-hell-with-rpy2-2-0-7-python-2-6-r-2-11-windows-7).
