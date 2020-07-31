
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtables

[![Build
Status](https://travis-ci.org/Roche/rtables.svg?branch=master)](https://travis-ci.org/Roche/rtables/)

## Reporting tables with R

The `rtables` R package is a prototype to create and display complex
tables with R. The cells in an `rtable` may contain any high-dimensional
data structure which can then be displayed with cell-specific formatting
instructions. Currently, `rtables` can be outputted in `ascii` and
`html`.

Some of our requirements and reasons to create this new table package
are:

  - multiple values displayed within a cell
  - values need to be programmatically accessible for cross-checking
  - flexible formatting (cell spans, rounding, alignment, etc.)
  - composition of an `rtable` object from other rtable objects
  - multiple output formats (html, ascii, latex, pdf)
  - flexible pagination

Note that the current state of `rtables` does not fulfill all of those
requirements. We are looking for collaborators, see the next section.

## Collaboration and Planned API Changes

The `rtables` package is currently in the process of being reworked to
address

The current state of the `rtables` functionality is a first rough
prototype. We will be changing the API significantly in the coming
months without maintaining backward compatibility. We will release the
`rtable` package on [CRAN](https://cran.r-project.org/) once we have a
stable version.

We are looking for collaborators to help define and shape the `rtables`
package. Please [contact me](mailto:adrian.waddell@roche.com) if you
would like to align your contributions to the current development plans
or feel free to fork the repository and to send us pull requests with
the suggested improvements.

## Installation

To install a frozen pre-release version of `rtables` based on the new
Layouting and Tabulation API as presented at user\!2020 and JSM2020 run
the following command in `R`:

``` r
devtools::install_github("roche/rtables", ref="v0.3.2.3")
```

To install the latest development version of the new test version of
`rtables` run

``` r
devtools::install_github("roche/rtables", ref = "gabe_tabletree_work")
```

## Usage

``` r
library(rtables)
library(dplyr)

## for simplicity grab non-sparse subset
ADSL = ex_adsl %>% filter(RACE %in% levels(RACE)[1:3])
biomarker_ave = function(x, ...) {
     val = if(length(x) > 0) round(mean(x), 2) else "no data"
     in_rows(
        "Biomarker 1 (mean)" = rcell(val)
     )
}

basic_table() %>%
  split_cols_by("ARM") %>%
  split_cols_by("BMRKR2") %>%
  add_colcounts() %>%
  split_rows_by("RACE", split_fun = trim_levels_in_group("SEX")) %>%
  split_rows_by("SEX") %>%
  summarize_row_groups() %>%
  analyze("BMRKR1", biomarker_ave) %>%
  build_table(ADSL)
#>                                          A: Drug X                            B: Placebo                           C: Combination           
#>                                LOW        MEDIUM        HIGH         LOW         MEDIUM       HIGH         LOW         MEDIUM        HIGH   
#>                               (N=45)      (N=35)       (N=46)       (N=42)       (N=48)      (N=31)       (N=40)       (N=39)       (N=47)  
#> --------------------------------------------------------------------------------------------------------------------------------------------
#> ASIAN                                                                                                                                       
#>   F                         13 (28.9%)   9 (25.7%)   19 (41.3%)   9 (21.4%)    18 (37.5%)    9 (29%)    13 (32.5%)   9 (23.1%)    17 (36.2%)
#>     Biomarker 1 (mean)         5.23        6.17         5.38         5.64         5.55        4.33         5.46         5.48         5.19   
#>   M                         8 (17.8%)     7 (20%)    10 (21.7%)   12 (28.6%)   10 (20.8%)   8 (25.8%)   5 (12.5%)    11 (28.2%)    16 (34%) 
#>     Biomarker 1 (mean)         6.77        6.06         5.54         4.9          4.98        6.81         6.53         5.47         4.98   
#>   U                          1 (2.2%)    1 (2.9%)      0 (0%)       0 (0%)       0 (0%)     1 (3.2%)      0 (0%)      1 (2.6%)     1 (2.1%) 
#>     Biomarker 1 (mean)         4.68         7.7       no data      no data      no data       6.97       no data       11.93         9.01   
#> BLACK OR AFRICAN AMERICAN                                                                                                                   
#>   F                         6 (13.3%)    3 (8.6%)    9 (19.6%)    6 (14.3%)    8 (16.7%)    2 (6.5%)    7 (17.5%)    4 (10.3%)     3 (6.4%) 
#>     Biomarker 1 (mean)         5.01         7.2         6.79         6.15         5.26        8.57         5.72         5.76         4.58   
#>   M                         5 (11.1%)    5 (14.3%)    2 (4.3%)     3 (7.1%)    5 (10.4%)    4 (12.9%)    4 (10%)     5 (12.8%)    5 (10.6%) 
#>     Biomarker 1 (mean)         6.92        5.82        11.66         4.46         6.14        8.47         6.16         5.25         4.83   
#>   U                           0 (0%)      0 (0%)       0 (0%)       0 (0%)       0 (0%)      0 (0%)      1 (2.5%)     1 (2.6%)      0 (0%)  
#>     Biomarker 1 (mean)       no data      no data     no data      no data      no data      no data       2.79         9.82       no data  
#>   UNDIFFERENTIATED           1 (2.2%)     0 (0%)       0 (0%)       0 (0%)       0 (0%)      0 (0%)       2 (5%)       0 (0%)       0 (0%)  
#>     Biomarker 1 (mean)         9.48       no data     no data      no data      no data      no data       6.46       no data      no data  
#> WHITE                                                                                                                                       
#>   F                         6 (13.3%)     7 (20%)     4 (8.7%)    5 (11.9%)    6 (12.5%)    6 (19.4%)    6 (15%)      3 (7.7%)     2 (4.3%) 
#>     Biomarker 1 (mean)         4.43        7.83         4.52         6.42         5.07        7.83         6.71         5.87         10.7   
#>   M                          4 (8.9%)    3 (8.6%)     2 (4.3%)    6 (14.3%)     1 (2.1%)    1 (3.2%)      2 (5%)     5 (12.8%)     3 (6.4%) 
#>     Biomarker 1 (mean)         5.81        7.23         1.39         4.72         4.58        12.87        2.3          5.1          5.98   
#>   U                          1 (2.2%)     0 (0%)       0 (0%)      1 (2.4%)      0 (0%)      0 (0%)       0 (0%)       0 (0%)       0 (0%)  
#>     Biomarker 1 (mean)         3.94       no data     no data        3.77       no data      no data     no data      no data      no data
```

# Acknowledgements

We would like to thank everyone who has made `rtables` a better project
by providing feedback and improving examples & vignettes. The following
list of contributors is alphabetical:

Daniel Sabanes Bove, Francois Collins, Tadeusz Lewandowski, Nick Paszty,
Nina Qi, Jana Stoilova, Heng Wang.

## Presentations

### New (Current) Layouting and Tabulation Framework (v.0.3+)

  - [useR\!2020 Presentation (on v0.3.1.1)
    July 2020](https://www.youtube.com/watch?v=CBQzZ8ZhXLA)

### v0.1.0 and previous

  - [Presentation on v0.1.0
    April 2018](https://docs.google.com/presentation/d/1bpdBDp4PZdZ4hCsfaPkAuHDVnJmtp7WBIZ19oKMDq0M/edit?usp=sharing)

  - [baselR November
    2017](https://docs.google.com/presentation/d/1V28AVo9aVNfw2FTuRgQyM4BJKalVQMFD8lKUD2KlzKI/edit?usp=sharing),
    this presentation was written for version `v0.0.1`
