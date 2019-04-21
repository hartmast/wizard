<!-- README.md is generated from README.Rmd. Please edit that file -->
wizard
======

A set of convenience functions for everyday programming. Work in progress. Currently just a bunch of functions that I use very often.

qbarplot
--------

The `qbarplot()` function allows for quickly generating ggplot2 barplots from raw dataframes:

``` r

qbarplot(diamonds, color, cut)
```

qbeeswarm
---------

The `qbeeswarm()` function allows for quickly generating ggplot2 beeswarm plots with overlaid boxplots. Depends on the ggbeeswarm package.

``` r

set.seed(200)
df <- data.frame(A = c(rep("high", 50), rep("lo", 50)),
                 B = round(runif(n = 100)*100))
qbeeswarm(df, x = A, y = B)
```

load\_packages
--------------

The `load_packages()` functions checks wher the packages specified as its arguments are installed and installs them if they are not. If `require = TRUE` (the default), they are also loaded in the process.

To be continued
---------------

More functions following soon!
