#' @export
#' @importFrom dplyr group_by_ group_by_prepare
group_by_.CrunchDataset <- function (.data, ..., .dots, add=FALSE) {
    groups <- group_by_prepare(.data, ..., .dots=.dots, add=add)
    missing_cols <- !(groups$groups %in% names(.data))
    if (any(missing_cols)) {
        err <- " is"
        if (sum(missing_cols) > 1) {
            err <- " are"
        }
        stop(paste0(groups$groups[missing_cols], collapse = ", "), 
            err, 
            " not present in the Dataset")
    }
    out <- GroupedCrunchDataset(groups$data)
    out@filter <- .data@filter ## TODO: why?
    out@groupBy <- groups$groups
    return(out)
}

#' @export
#' @importFrom dplyr group_by_ groups
groups.GroupedCrunchDataset <- function (x) x@groupBy

#' @export
groups.CrunchDataset <- function (x) list()

#' @export
#' @importFrom dplyr  group_vars
group_vars.GroupedCrunchDataset <- function (x) as.character(x@groupBy)

#' @export
group_vars.CrunchDataset <- function (x) NULL




#' @export
#' @importFrom dplyr ungroup
ungroup.CrunchDataset <- function (x, ...) x

#' @export
#' @importFrom crunch CrunchDataset
ungroup.GroupedCrunchDataset <- function (x, ...) {
    out <- CrunchDataset(x)
    out@filter <- x@filter ## TODO: again, why?
    return(out)
}

#' @export
#' @importFrom dplyr tbl_vars
tbl_vars.CrunchDataset <- function (x) names(x)

