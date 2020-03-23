#' Filter a Crunch dataset (deprecated)
#'
#' This function is deprecated, use `filter()` instead.
#' Applies a `CrunchLogicalExpression` filter to a
#' `CrunchDataset`. It's a "tidy" way of doing `ds[ds$var == val,]`.
#'
#' @param .data A `CrunchDataset`
#' @param ... filter expressions
#' @param .dots More dots!
#' @return `.data` with the filter expressions applied.
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


#' Filter a Crunch dataset
#'
#' This function applies a `CrunchLogicalExpression` filter to a
#' `CrunchDataset`. It's a "tidy" way of doing `ds[ds$var == val,]`.
#'
#' @param .data A `CrunchDataset`
#' @param ... filter expressions
#' @param .preserve Relevant when the `.data` input is grouped. If 
#' `.presrve = FALSE` (the default), the grouping structure is recalculated
#' based on the resulting data, otherwise the grouping is kept as is.
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
#' @importFrom rlang enquos eval_tidy
filter.CrunchDataset <- function(.data, ..., .preserve = FALSE) {
    if (missing(...)) return(.data)
    if (.preserve) stop("`.preserve` is not supported by CrunchDatasets.")

    dots <- enquos(...)
    filts <- lapply(dots, function(dot) eval_tidy(dot, crunch_var_list(.data)))

    return(.data[Reduce("&", filts),])
}

# rlang functions like eval_tidy don't work on CrunchDataset objects directly,
# instead they expect a list object with references to each 
# variable and names of the variables.
# A nice consequence of R's lazy evaluation is that this doesn't
# actually make the requests for each variable
crunch_var_list <- function(dataset) {
    setNames(lapply(names(dataset), function(x) dataset[[x]]), names(dataset))
}