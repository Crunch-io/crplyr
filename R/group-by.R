#' @export
#' @importFrom dplyr group_by group_by_prepare
group_by.CrunchDataset <- function (.data, ..., add=FALSE) {
    # Note that group_by_prepare will try to mutate() if you request vars that
    # don't exist, and we don't support mutate yet. Consider capturing the
    # error message and throwing something more useful.
    groups <- group_by_prepare(.data, ..., add=add)
    missing_cols <- !(groups$groups %in% aliases(allVariables(.data)))
    if (any(missing_cols)) {
        err <- " is"
        if (sum(missing_cols) > 1) {
            err <- " are"
        }
        stop(
            paste0(groups$groups[missing_cols], collapse = ", "),
            err,
            " not present in the Dataset",
            call.=FALSE
        )
    }
    out <- GroupedCrunchDataset(groups$data)
    out@groupBy <- groups$groups
    return(out)
}

#' @export
#' @importFrom dplyr group_by_
group_by_.CrunchDataset <- function (.data, ..., .dots, add = FALSE) {
    stop(
        "The group_by_() function is no longer supported. ",
        "Please use group_by() instead.",
        call.=FALSE
    )
}

#' @export
#' @importFrom dplyr groups
groups.GroupedCrunchDataset <- function (x) x@groupBy

#' @export
groups.CrunchDataset <- function (x) list()

#' @export
#' @importFrom dplyr group_vars
group_vars.GroupedCrunchDataset <- function (x) as.character(x@groupBy)

#' @export
group_vars.CrunchDataset <- function (x) NULL

#' @export
#' @importFrom dplyr ungroup
ungroup.CrunchDataset <- function (x, ...) x

#' @export
#' @importFrom crunch CrunchDataset
ungroup.GroupedCrunchDataset <- function (x, ...) CrunchDataset(x)

#' @export
#' @importFrom dplyr tbl_vars
tbl_vars.CrunchDataset <- function (x) names(x)
