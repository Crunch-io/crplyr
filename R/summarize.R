#' @export
#' @importFrom dplyr summarise_
#' @importFrom crunch crtabs
summarise_.CrunchDataset <- function (.data, ..., .dots) {
    dots <- all_dots(.dots, ..., all_named = TRUE)
    fmla <- dots_to_formula(dots, groups(.data))
    out <- as_tibble(crtabs(fmla, data=.data))
    ## Sort the return columns based on the request order
    out <- out[, c(setdiff(names(out), names(dots)), names(dots))]
    return(out)
}

#' @importFrom stats as.formula
dots_to_formula <- function (dots, grps=list()) {
    as.formula(paste(dots_to_LHS(dots), groups_to_RHS(grps), sep=" ~ "))
}

dots_to_LHS <- function (dots) {
    exprs <- dots_to_list(dots)
    terms <- paste(names(exprs), exprs, sep="=", collapse=", ")
    return(paste0("list(", terms, ")"))
}

groups_to_RHS <- function (grps) {
    if (length(grps)) {
        return(paste(grps, collapse="+"))
    } else {
        ## Ungrouped
        return("1")
    }
}

dots_to_list <- function (dots) lapply(dots, function (ex) deparse(ex$expr))
