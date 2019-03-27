#' Filter a Crunch dataset
#'
#' This function applies a `CrunchLogicalExpression` filter to a
#' `CrunchDataset`. It's a "tidy" way of doing `ds[ds$var == val,]`.
#'
#' @param .data A `CrunchDataset`
#' @param ... filter expressions
#' @param .dots More dots!
#' @return `.data` with the filter expressions applied.
#' @name filter
#' @examples
#' \dontrun{
#' ds %>%
#'    select(cyl, gear) %>%
#'    filter(cyl > 4) %>%
#'    collect()
#' }
#' @export
#' @importFrom dplyr filter_
#' @importFrom lazyeval all_dots
filter_.CrunchDataset <- function (.data, ..., .dots) {
    # TODO: drop filter_ for filter?
    dots <- all_dots(.dots, ...)
    env <- as.environment(.data)
    filts <- lapply(dots, function (ex) eval(ex$expr, envir=env))
    return(.data[Reduce("&", filts),])
}
