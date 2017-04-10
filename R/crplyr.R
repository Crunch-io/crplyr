# Here's a good place to put your top-level package documentation

.onAttach <- function (lib, pkgname="crplyr") {
    ## Put stuff here you want to run when your package is loaded
    invisible()
}

## Use existing methods
# -   `select()` picks variables based on their names.
# -   `filter()` picks cases based on their values.

## Add subclass for this?
# -   `mutate()` adds new variables that are functions of existing variables
# -   `group_by()` which allows you to perform any operation "by group"

## Wrap crtabs/as.data.frame and return tibble
# -   `summarise()` reduces multiple values down to a single summary.
# -   `collect()`

## Don't bother?
# -   `arrange()` changes the ordering of the rows.

## as_tbl for CrunchCube
## `locally` context to modify objects but not send to server?
