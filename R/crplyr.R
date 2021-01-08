# Here's a good place to put your top-level package documentation

#' @importFrom crunch set_crunch_config
.onLoad <- function (lib, pkgname="crplyr") {
    # Re-set the API config so that crplyr shows up in the user-agent string
    set_crunch_config(update=TRUE)
    return(invisible())
}

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(".")

# Due to CRAN rules (grumble grumble), we want to have httptest as an Imported
# package instead of a Suggested package (because tests can't fail if it's
# not available and only in Suggests), so we need to use it somewhere so that
# we don't get a different NOTE about not using an Imported package.
ignore_me <- function() {
    httptest::change_state()
}
