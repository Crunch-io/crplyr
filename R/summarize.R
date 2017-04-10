#' @export
#' @importFrom dplyr summarise_
summarise_.CrunchDataset <- function (.data, ..., .dots) {
    dots <- all_dots(.dots, ..., all_named = TRUE)
    fmla <- dots_to_formula(dots)
    return(as_tibble(crtabs(fmla, data=.data)))
}

dots_to_formula <- function (dots) {
    exprs <- lapply(dots, function (ex) deparse(ex$expr))
    terms <- paste(names(exprs), exprs, sep="=", collapse=", ")
    ## TODO: look for "groups", put them on RHS
    fmla <- as.formula(paste0("list(", terms, ") ~ 1"))
    return(fmla)
}
