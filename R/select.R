#' @export
#' @importFrom dplyr select select_vars
select.CrunchDataset <- function (.data, ...) {
    # Use allVariables so we can include hidden variables
    vars <- select_vars(aliases(allVariables(.data)), ...)
    return(.data[vars])
}

#' @export
#' @importFrom dplyr select_
select_.CrunchDataset <- function (.data, ..., .dots) {
    stop("The select_() function is no longer supported, please use select() instead.")
}
