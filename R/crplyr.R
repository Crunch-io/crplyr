# Here's a good place to put your top-level package documentation

#' @importFrom crunch set_crunch_config
.onLoad <- function (lib, pkgname="crplyr") {
    # Re-set the API config so that crplyr shows up in the user-agent string
    set_crunch_config(update=TRUE)
    return(invisible())
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(".")
