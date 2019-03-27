#' Mutate Crunch datasets (not implemented)
#'
#' Just a method that returns a nicer error message. `mutate()` hasn't been
#' implemented yet. You can, however, derive expressions on the fly in
#' [summarize()].
#' @param .data A crunch Dataset
#' @param ... Other arguments, currently ignored
#' @name mutate
#' @export
#' @importFrom dplyr mutate
mutate.CrunchDataset <- function (.data, ...) {
    stop(
        "`mutate()` is not currently supported in crplyr. You can, however, ",
        "derive expressions on the fly in `summarize()`.",
        call.=FALSE
    )
}

#' @export
#' @importFrom dplyr mutate_
mutate_.CrunchDataset <- function (.data, ...) mutate.CrunchDataset(.data)
