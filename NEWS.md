# crplyr 0.3.7 (development version)
* Allow tests to run with only required packages (fixes CRAN note).

# crplyr 0.3.6
* Another fix in vignette for upcoming version of crunch.

# crplyr 0.3.5
* Small update to testing infrastructure to accomodate upcoming crunch update.

# crplyr 0.3.4
* Converting CrunchCube results to the tibble like object "tbl_crunch_cube" is now done via `as_cr_tibble()` and `as_tibble()` always returns an actual tibble.
* Updates for upcoming dplyr 1.0 release

# crplyr 0.3.2
* Fixes  to get back on CRAN

# crplyr 0.3.1 
* New maintainer

# crplyr 0.3.0

* New `autoplot()` methods make it easy to plot crunch variables and cubes as well as cube calculations (proportions from `prop.table()` margins from `margin.table()`, etc.). See `vignette("plotting", package="crplyr")` for discussion and examples.
* `as_tibble()` and `summarize()` return full underlying cube representations, where multiple-response items are represented as an array of multiple dichotomous choices (selected, not-selected, missing) for each item.
* `select()`, `group_by()`, and `collect()` now correctly support taking hidden variables (#6, #13)
* `collect()` uses `crunch::as.data.frame()` export, which should be faster especially for larger data pulls.
* Internal methods `summarize_()`, `select_()`, and `group_by_()`, deprecated in `dplyr`, now error.
* Improved documentation and error messages (#5, #9).

# crplyr 0.2.0

* Added `collect()` method, which pulls the requested columns of data from the server.
* `summarize()` and `as_tibble.CrunchCube()` methods now better handle array and multiple-response data and include metadata on which dimension values should be interpreted as missing.
* `unweighted_n()` aggregation method for `summarize()`, which returns the unweighted counts even when the dataset has a weight applied.

# crplyr 0.1.4

* Update tests to match `crunch` 1.18.0.

# crplyr 0.1.2

* Update tests to use new setup provided in `crunch` 1.17.0.

# crplyr 0.1.0

Initial implementation of `dplyr` interface for Crunch, including

* `select()`, `filter()`, `group_by()`, and `summarize()` methods for `crunch::CrunchDataset()` objects
* `as_tibble()` method for `crunch::CrunchCube()` objects
