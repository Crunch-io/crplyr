#' @export
#' @importFrom dplyr summarise_
#' @importFrom crunch crtabs
summarise_.CrunchDataset <- function (.data, ..., .dots) {
    dots <- all_dots(.dots, ..., all_named = TRUE)
    fmla <- dots_to_formula(dots)
    out <- as_tibble(crtabs(fmla, data=.data))
    ## Sort the return columns based on the request order
    out <- out[, c(setdiff(names(out), names(dots)), names(dots))]
    return(out)
}

#' @importFrom stats as.formula
dots_to_formula <- function (dots) {
    exprs <- lapply(dots, function (ex) deparse(ex$expr))
    terms <- paste(names(exprs), exprs, sep="=", collapse=", ")
    ## TODO: look for "groups", put them on RHS
    fmla <- as.formula(paste0("list(", terms, ") ~ 1"))
    return(fmla)
}
