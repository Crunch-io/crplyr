#' Select columns from a Crunch dataset
#'
#' This function uses "tidy select" methods of subsetting the columns of a
#' dataset. It's another way of doing `ds[,vars]`.
#'
#' @param .data A `CrunchDataset`
#' @param ... names of variables in `.data` or other valid selection functions,
#' passed to `dplyr::select_vars()`
#' @return `.data` with only the selected variables.
#' @name select
#' @examples
#' \dontrun{
#' ds %>%
#'    select(contains("ear")) %>%
#'    filter(gear > 4) %>%
#'    collect()
#' }
#' @export
#' @importFrom dplyr select select_vars
#' @importFrom crunch aliases allVariables
select.CrunchDataset <- function (.data, ...) {
    # Use allVariables so we can include hidden variables
    vars <- select_vars(aliases(allVariables(.data)), ...)
    return(.data[vars])
}

#' @export
#' @importFrom dplyr select_
select_.CrunchDataset <- function (.data, ..., .dots) {
    stop(
        "The select_() function is no longer supported. ",
        "Please use select() instead.",
        call.=FALSE
    )
}
