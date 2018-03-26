
<!-- README.md is generated from README.Rmd. Please edit that file -->
rtables
=======

[![Build Status](https://travis-ci.org/Roche/rtables.svg?branch=master)](https://travis-ci.org/Roche/rtables/)

[project webpage](https://roche.github.io/rtables/)

Reporting tables with R
-----------------------

The `rtables` R package is a prototype to create and display complex tables with R. The cells in an `rtable` may contain any high-dimensional data structure which can then be displayed with cell-specific formating instructions. Currently `rtables` can be outputted in `ascii` and `html`.

Some of our requirements and reasons to create this new table package are:

-   multiple values displayed within a cell
-   values need to be programmatically accessible for cross-checking
-   flexible formatting (cell spans, rounding, alignment, etc.)
-   composition of an rtable from other rtable objects
-   multiple output formats (html, ascii, latex, pdf)
-   flexible pagination

Note that the current state of `rtables` does not fulfill all of those requirements. We are looking for collaborators, see the next section.

Collaboration and Planned API Changes
-------------------------------------

The current state of the `rtables` functionality is a first rough prototype. We will be changing the API significantly in the coming months without maintaining backwards compatibility. We will release the `rtable` package on [CRAN](https://cran.r-project.org/) once we have a stable version.

We are looking for collaborators to help define and shape the `rtables` package. Please [contact me](mailto:adrian.waddell@roche.com) if you would like to align your contributions to the current development plans or feel free to fork the repository and to send us pull requests with the suggested improvements.

Installation
------------

To install the `rtables` package run the following command in `R`:

``` r
devtools::install_github("roche/rtables")
```

Usage
-----

``` r
library(rtables)
tbl <- rtable(
  header = c("Treatement\nN=100", "Comparison\nN=300"),
  format = "xx (xx.xx%)",
  rrow("A", c(104, .2), c(100, .4)),
  rrow("B", c(23, .4), c(43, .5)),
  rrow(),
  rrow("this is a very long section header"),
  rrow("estimate", rcell(55.23, "xx.xx", colspan = 2)),
  rrow("95% CI", indent = 1, rcell(c(44.8, 67.4), format = "(xx.x, xx.x)", colspan = 2))
)
tbl
#>                                           Treatement        Comparison
#>                                             N=100             N=300   
#> ----------------------------------------------------------------------
#> A                                         104 (20%)         100 (40%) 
#> B                                          23 (40%)          43 (50%) 
#> 
#> this is a very long section header
#> estimate                                             55.23            
#>   95% CI                                          (44.8, 67.4)
```

Presentations
-------------

-   [baselR November 2017](https://docs.google.com/presentation/d/1V28AVo9aVNfw2FTuRgQyM4BJKalVQMFD8lKUD2KlzKI/edit?usp=sharing)
