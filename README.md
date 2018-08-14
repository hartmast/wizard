<!-- README.md is generated from README.Rmd. Please edit that file -->
wizard
======

A set of convenience functions for everyday programming. Work in progress. Currently just a bunch of functions that I use very often.

qbarplot
--------

The `qbarplot()` function allows for quickly generating ggplot2 functions from dataframes:

``` r

qbarplot(diamonds, color, cut)
```

load\_packages
--------------

The `load_packages()` functions checks wher the packages specified as its arguments are installed and installs them if they are not. If `require = TRUE` (the default), they are also loaded in the process.

To be continued
---------------

More functions following soon!
