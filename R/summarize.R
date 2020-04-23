#' Aggregate a Crunch dataset
#'
#' This is an alternate interface to `crunch::crtabs()` that, in addition to
#' being "tidy", makes it easier to query multiple measures at the same time.
#'
#' Note that while `mutate()` is not generally supported in `crplyr`, you can
#' derive expressions on the fly in `summarize()`.
#'
#' @param .data A `CrunchDataset`
#' @param ... named aggregations to include in the resulting table.
#' @return A `tbl_crunch_cube` or `cr_tibble` of results. This subclass
#' of `tibble` allows `ggplot2::autoplot` to work, but can get in the way
#' in some tidyverse operations. You may wish to convert to a tibble using 
#' `as_tibble()`.
#' @name summarize
#' @examples
#' \dontrun{
#' ds %>%
#'     filter(cyl == 6) %>%
#'     group_by(vs) %>%
#'     summarize(hp=mean(hp), sd_hp=sd(hp), count=n())
#' }
#' @export
#' @importFrom dplyr bind_cols summarise select
#' @importFrom purrr map_chr map_df
#' @importFrom crunch crtabs
#' @importFrom rlang enquos quo_text
summarise.CrunchDataset <- function (.data, ...) {
    dots <- enquos(...)
    dots_text <- lapply(dots, quo_text)
    unweighted <- dots_text == "unweighted_n()"
    unweighted_n_measures <- dots[unweighted]
    measures <- dots[!unweighted]
    fmla <- dots_to_formula(measures, groups(.data))

    if (length(measures) == 0 && length(groups(.data)) == 0) {
        # When there are no groups or summary functions, we can't naturally
        # use crtabs, but unweighted_n() is equivalent to nrow(ds), so use that.
        #
        # We're using map_df because it's possible that the user asks for
        # several unweighted_n's in the same summarize call. map_df here
        # generalizes to 0, 1, or many.
        #
        # TODO: make a cr_tibble so we have consistent return types?
        out <- map_df(unweighted_n_measures, ~nrow(.data))
    } else {
        # The usual case: call crtabs.
        out <- as_cr_tibble(crtabs(fmla, data=.data))
        # If unweighted_n() is requested, map it to the requested column names
        # from where it naturally appears in the tbl as "row_count". Then
        # remove "row_count"
        if (any(unweighted)) {
            unweighted_n <- map_df(unweighted_n_measures, ~ out$row_count)
            old_attr <- attributes(out)
            out$row_count <- NULL
            out <- bind_cols(as_tibble(out), unweighted_n)
            out <- as_cr_tibble(
                out, 
                cube_metadata = old_attr$cube_metadata,
                types = old_attr$types,
                useNA = old_attr$useNA
            )
        } else {
            out$row_count <- NULL    
        }
        
    }

    # Some cubes, like those produced from a summarize with no grouping,
    # don't have an "is_missing" column, so we need this
    # intersect to handle cubes whether or not they have the column
    names <- intersect(
        c(as.character(groups(.data)), "is_missing", names(dots)),
        names(out)
    )

    # sort the return columns based on the request order
    out <- out[, names]
    return(out)
}

#' @export
#' @importFrom dplyr summarise_
summarise_.CrunchDataset <- function (.data, ..., .dots) {
    stop(
        "The summarise_() function is no longer supported. ",
        "Please use summarise() instead.",
        call.=FALSE
    )
}


#' Return the unweighted counts from summarize
#'
#' This function allows you to return the unweighted counts from a Crunch dataset
#' or grouped crunch dataset. It can only be used from within a `summarise()`
#' call. If your dataset is unweighted, then unweighted_n() is equivalent to n().
#'
#' @export
#' @examples
#' \dontrun{
#' ds %>%
#'    group_by(cyl) %>%
#'    summarize(
#'        raw_counts = unweighted_n(),
#'        mean = mean(wt)
#'    )
#' }
unweighted_n <- function () {
    stop(
        "This function cannot be called outside of a summarize call.",
        .call = FALSE
    )
}

#' @importFrom stats as.formula
dots_to_formula <- function (dots, grps=list()) {
    as.formula(paste(dots_to_LHS(dots), groups_to_RHS(grps), sep = " ~ "))
}

dots_to_LHS <- function (dots) {
    if (length(dots) == 0) {
        return("")
    }
    exprs <- dots_to_list(dots)
    terms <- paste(names(exprs), exprs, sep = "=", collapse = ", ")
    return(paste0("list(", terms, ")"))
}

groups_to_RHS <- function (grps) {
    if (length(grps)) {
        return(paste(grps, collapse = "+"))
    } else {
        ## Ungrouped
        return("1")
    }
}

#' @importFrom rlang quo_text
dots_to_list <- function (dots) lapply(dots, function (ex) quo_text(ex))
