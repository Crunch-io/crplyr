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
#' @param .add Logical: add the variables in `...` to any existing grouping
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
group_by.CrunchDataset <- function (.data, ..., .add=FALSE) {
    groups <- crunch_group_by_prepare(.data, ..., .add=.add)
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
#' @importFrom crunch aliases allVariables
tbl_vars.CrunchDataset <- function (x) names(x)

# Adapted from dplyr::group_by_prepare, but:
# 1) Doesn't allow mutates in group_by with a nice error
# 2) Doesn't use tbl_vars so that it can get hidden variables
#' @importFrom lifecycle deprecated deprecate_soft deprecate_warn
#' @importFrom rlang enquos caller_env quo_is_missing have_name exprs_auto_name abort syms
#' @importFrom dplyr union setdiff 
crunch_group_by_prepare <- function(
    .data, 
    ..., 
    .add = FALSE, 
    .dots = deprecated(), 
    add = deprecated()
) {
    if (!missing(add)) {
        deprecate_warn("1.0.0", "dplyr::group_by(add = )", 
                                  "dplyr::group_by(.add = )")
        .add <- add
    }

    new_groups <- enquos(...)

    new_groups <- new_groups[!vapply(new_groups, quo_is_missing, logical(1))]
    is_symbol <- vapply(new_groups, quo_is_variable_reference, logical(1))
    needs_mutate <- have_name(new_groups) | !is_symbol
    
    if (any(needs_mutate)) {
        stop(
            "Cannot create variables in a `group_by()` statement. You can, however, derive ",
            "expressions on the fly in `summarize()`.",
            call. = FALSE)
    }
    out <- .data

    group_names <- names(exprs_auto_name(new_groups))
    if (.add) {
        group_names <- union(group_vars(.data), group_names)
    }

    unknown <- setdiff(group_names, aliases(allVariables(out)))
    if (length(unknown) > 0) {
        abort(c(
            "Must group by variables found in `.data`",
            paste0("Column `", unknown, "` is not found")
        ))
    }

    list(
        data = out,
        groups = syms(group_names),
        group_names = group_names
    )
}


# Internal function from dplyr
#' @importFrom rlang quo_is_symbol quo_is_call quo_get_expr node_cadr sym node_car node_cdr is_symbol is_string
quo_is_variable_reference <- function(quo) {
    if (quo_is_symbol(quo)) {
        return(TRUE)
    }

    if (quo_is_call(quo, n = 2)) {
        expr <- quo_get_expr(quo)
        
        if (node_cadr(expr) == sym(".data")) {
            fun <- node_car(expr)
            param <- node_cadr(node_cdr(expr))
            
            if (fun == sym("$") && (is_symbol(param) || is_string(param))) {
                return(TRUE)
            }
            
            if (fun == sym("[[") && is_string(param)) {
                return(TRUE)
            }
        }
    }

    FALSE
}