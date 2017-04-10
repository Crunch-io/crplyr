#' @export
#' @importFrom dplyr select_ select_vars_
#' @importFrom lazyeval all_dots
select_.CrunchDataset <- function (.data, ..., .dots) {
    dots <- all_dots(.dots, ...)
    vars <- select_vars_(names(.data), dots)
    return(.data[vars])
}

## Future dplyr release?
# select.CrunchDataset <- function (.data, ...) {
#     vars <- select_vars(names(.data), ...)
#     return(.data[vars])
# }
