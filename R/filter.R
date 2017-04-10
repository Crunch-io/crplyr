#' @export
#' @importFrom dplyr filter_
filter_.CrunchDataset <- function (.data, ..., .dots) {
    dots <- all_dots(.dots, ...)
    env <- as.environment(.data)
    filts <- lapply(dots, function (ex) eval(ex$expr, envir=env))
    return(.data[Reduce("&", filts),])
}
