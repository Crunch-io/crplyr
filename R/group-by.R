#' Group-by for Crunch datasets
#'
#' `group_by()` sets grouping variables that affect what [summarize()] computes.
#' `ungroup()` removes any grouping variables.
#'
#' Note that `group_by()` only supports grouping on variables that exist in the
#' dataset, not ones that are derived on the fly. `dplyr::group_by()` supports
#' that by calling `mutate()` internally, but `mutate` is not yet supported in
#' `crplyr`.
#'
#' @param .data For `group_by()`, a Crunch Dataset
#' @param x For `ungroup()`, a Crunch Dataset
#' @param ... references to variables to group by, passed to
#' [dplyr::group_by_prepare()]
#' @param add Logical: add the variables in `...` to any existing grouping
#' variables, or replace them (the default).
#' @return `group_by()` returns a `GroupedCrunchDataset` object (a
#' `CrunchDataset` with grouping annotations). `ungroup()` returns a
#' `CrunchDataset`.
#' @name group_by
#' @examples
#' \dontrun{
#' ds %>%
#'    group_by(cyl) %>%
#'    select(cyl, gear) %>%
#'    collect()
#' }
#' @export
#' @importFrom dplyr group_by group_by_prepare
group_by.CrunchDataset <- function (.data, ..., add=FALSE) {
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

#' @name group_by
#' @export
#' @importFrom dplyr ungroup
#' @importFrom crunch CrunchDataset
ungroup.CrunchDataset <- function (x, ...) CrunchDataset(x)

#' @export
#' @importFrom dplyr tbl_vars
tbl_vars.CrunchDataset <- function (x) names(x)
