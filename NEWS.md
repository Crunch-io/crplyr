# crplyr 0.2.0
* Added `collect` method, which pulls the requested columns of data from the server.
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
