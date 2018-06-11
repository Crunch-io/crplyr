#' @export
#' @importFrom dplyr bind_cols summarise_ select
#' @importFrom purrr map_chr map_df
#' @importFrom crunch crtabs
summarise_.CrunchDataset <- function (.data, ..., .dots) {
    dots <- all_dots(.dots, ..., all_named = TRUE)
    unweighted <- dots %>% map_chr(~as.character(.$expr)[[1]]) == "unweighted_n"
    counts <- dots[unweighted]
    non_counts <- dots[!unweighted]
    fmla <- dots_to_formula(non_counts, groups(.data))
    
    # When there are no groups or summary functions, we can't 
    # use crtabs to get a cube, but unweighted_n() is equivalent to nrow(ds)
    # Otherwise we can use crtabs to get the cube and extract the unweighted counts
    # from the cube. This doesn't work for weighted counts because they 
    # are not included in some cubes. So in that case we rely on the the server
    # `n` function. 
    if (length(non_counts) == 0 && length(groups(.data)) == 0) {
        out <- map_df(counts, ~nrow(.data))
    } else {
        cube <- crtabs(fmla, data=.data)
        
        # map_df is used because the user could be assigning unweighted counts
        # to several variables. 
        unweighted_n <-  map_df(counts, ~cube@arrays$.unweighted_counts)
        out <- cube %>% 
            as_tibble() %>% 
            select(-.data$row_count) %>% 
            bind_cols(unweighted_n)
    } 
    
    # Some cubes, like those produced from a summarize with no grouping,
    #  don't have an "is_missing" column, so we need this 
    # intersect to handle cubes whether or not they have the column
    names <- intersect(
        c(as.character(groups(.data)), "is_missing", names(dots)),
        names(out)
    )

    # sort the return columns based on the request order
    out <- out[, names]
    return(out)
}


#' Return the unweighted counts from summarize
#' 
#' This function allows you to return the unweighted counts from a Crunch dataset
#' or grouped crunch dataset. Currently it can only be used from within a summarize
#' call.
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
unweighted_n <- function() {
    stop("This function cannot be called outside of a summarize call.")
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

dots_to_list <- function (dots) lapply(dots, function (ex) deparse(ex$expr))
