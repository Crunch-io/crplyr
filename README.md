# crplyr: A 'dplyr' Interface for Crunch

[![Build Status](https://travis-ci.org/Crunch-io/crplyr.png?branch=master)](https://travis-ci.org/Crunch-io/crplyr)  [![codecov](https://codecov.io/gh/Crunch-io/crplyr/branch/master/graph/badge.svg)](https://codecov.io/gh/Crunch-io/crplyr) [![Build status](https://ci.appveyor.com/api/projects/status/wjc1inaakamltdq1/branch/master?svg=true)](https://ci.appveyor.com/project/nealrichardson/crplyr/branch/master)

[dplyr](https://github.com/tidyverse/dplyr) defines "a grammar of data manipulation" popular among R users. In order to facilitate analysis of datasets hosted by Crunch, this package implements 'dplyr' methods on top of the Crunch backend. The usual methods "select", "filter", "mutate", "group_by", and "summarize" are implemented in such a way as to perform as much computation on the server and pull as little data locally as possible.

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

The pre-release version of the package can be pulled from GitHub using the [devtools](https://github.com/hadley/devtools) package:

    # install.packages("devtools")
    devtools::install_github("Crunch-io/crplyr")

## For developers

The repository includes a Makefile to facilitate some common tasks.

### Running tests

`$ make test`. Requires the [httptest](https://github.com/nealrichardson/httptest) package. You can also specify a specific test file or files to run by adding a "file=" argument, like `$ make test file=logging`. `test_package` will do a regular-expression pattern match within the file names. See its documentation in the `[testthat](https://github.com/hadley/testthat)` package.

### Updating documentation

`$ make doc`. Requires the [roxygen2](https://github.com/klutometis/roxygen) package.
