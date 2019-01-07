# Here's a good place to put your top-level package documentation

.onAttach <- function (lib, pkgname="crplyr") {
    ## Put stuff here you want to run when your package is loaded

    ## (try) to add the crplyr useragent, though if that fails for any reason
    ## (like using an old version of the crunch package, don't fail totally)
    cr_useragent <- paste0("crplyr", "/", as.character(packageVersion("crplyr")))
    try({add_to_crunch_useragent(cr_useragent)})

    return(invisible())
}

## TODO
# -   `mutate()` adds new variables that are functions of existing variables
# -   `collect()`

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))