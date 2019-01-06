#' @export
#' @importFrom dplyr select select_vars
select.CrunchDataset <- function (.data, ...) {
    vars <- select_vars(names(.data), ...)
    return(.data[vars])
}

#' @export
#' @importFrom dplyr select_
select_.CrunchDataset <- function (.data, ..., .dots) {
    stop("The select_() function is no longer supported, please use select() instead.")
}