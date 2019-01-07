# Here's a good place to put your top-level package documentation

.onAttach <- function (lib, pkgname="crplyr") {
    ## Put stuff here you want to run when your package is loaded

    # add to crunch::crunchUserAgent()
    invisible()
}

## TODO
# -   `mutate()` adds new variables that are functions of existing variables
# -   `collect()`

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))