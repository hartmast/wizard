<!-- README.md is generated from README.Rmd. Please edit that file -->
wizard
======

A set of convenience functions for everyday programming and for
visualization. Work in progress. Currently just a bunch of functions
that I use very often.

qbarplot
--------

The `qbarplot()` function allows for quickly generating ggplot2 barplots
from raw dataframes:

``` r

qbarplot(diamonds, color, cut)
```

qbeeswarm
---------

The `qbeeswarm()` function allows for quickly generating ggplot2
beeswarm plots with overlaid boxplots. Depends on the ggbeeswarm
package.

``` r

set.seed(200)
df <- data.frame(A = c(rep("high", 50), rep("lo", 50)),
                 B = round(runif(n = 100)*100))
qbeeswarm(df, x = A, y = B)
```

pretty\_df
----------

The `pretty_df()` function formats the output of dataframes in a way
that there are two digits after the decimal separator. Note that the
“prettified” version will often not be printed in the R output but it
will appear e.g. if you export the data to a CSV file.

``` r

df <- data.frame(a = c("a", "b", "c"),
                 b = c(0.89469435394539469569, 123684.2683523825, 0.00000005))

p_df <- pretty_df(df)
write.csv(p_df, "prettydataframe.csv")
```

load\_packages
--------------

The `load_packages()` function checks whether the packages specified as
its arguments are installed and installs them if they are not. If
`require = TRUE` (the default), they are also loaded in the process.

To be continued
---------------

More functions following soon!
