# crplyr: A 'dplyr' Interface for Crunch

<!-- badges: start -->
[![R build status](https://github.com/Crunch-io/crplyr/workflows/R-CMD-check/badge.svg)](https://github.com/Crunch-io/crplyr/actions)
[![codecov](https://codecov.io/gh/Crunch-io/crplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/Crunch-io/crplyr)
[![cran](https://www.r-pkg.org/badges/version-last-release/crplyr)](https://cran.r-project.org/package=crplyr)
<!-- badges: end -->

[dplyr](https://dplyr.tidyverse.org/) defines "a grammar of data manipulation" popular among R users. In order to facilitate analysis of datasets hosted by Crunch, this package implements 'dplyr' methods on top of the Crunch backend. The usual methods "select", "filter", "group_by", "summarize", and "collect" are implemented in such a way as to perform as much computation on the server and pull as little data locally as possible.

With a local `data.frame`, you might chain together a series of manipulations and create a table, such as:

    > library(dplyr)
    > data(mtcars)
    > mtcars %>%
        filter(vs == 1) %>%
        group_by(gear) %>%
        summarize(horses=mean(hp), sd_horses=sd(hp), count=n())

    ## # A tibble: 3 × 4
    ##    gear horses sd_horses count
    ##   <dbl>  <dbl>     <dbl> <int>
    ## 1     3  104.0  6.557439     3
    ## 2     4   85.4 26.596575    10
    ## 3     5  113.0        NA     1

With `crplyr`, you can do the same operations, except that the dataset you're working with sits in the Crunch platform, and Crunch is doing the aggregations in the cloud:

    > library(crplyr)
    > login()
    [crunch] > mtcars <- loadDataset("mtcars from R")
    [crunch] > mtcars %>%
        filter(vs == 1) %>%
        group_by(gear) %>%
        summarize(horses=mean(hp), sd_horses=sd(hp), count=n())

    ## # A tibble: 3 × 4
    ##    gear horses sd_horses count
    ##  <fctr>  <dbl>     <dbl> <dbl>
    ## 1     3  104.0  6.557439     3
    ## 2     4   85.4 26.596575    10
    ## 3     5  113.0        NA     1

Obviously, the fact that the calculations in `crplyr` are happening remotely doesn't matter as much when working with a tiny dataset like "mtcars", but Crunch allows you to work with datasets larger than can fit in memory on your machine, and it enables you to collaborate naturally with others on the same dataset.

## Installing

Install the CRAN release of `crplyr` with

    install.packages("crplyr")

The pre-release version of the package can be pulled from GitHub using the [remotes](https://remotes.r-lib.org/) package:

    # install.packages("remotes")
    remotes::install_github("Crunch-io/crplyr")

## For developers

The repository includes a Makefile to facilitate some common tasks, if you're into that sort of thing.

### Running tests

`$ make test`. Requires the [httptest](https://enpiar.com/r/httptest/) package. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=select`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the [testthat](https://testthat.r-lib.org/) package.

### Updating documentation

`$ make doc`. Requires the [roxygen2](https://github.com/r-lib/roxygen2) package.
