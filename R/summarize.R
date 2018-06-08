#' @export
#' @importFrom dplyr summarise_ select
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
    # from the cube. 
    if (length(non_counts) == 0 && length(groups(.data)) == 0) {
        n_row <- nrow(.data)
        out <- map_df(counts, ~n_row)
    } else {
        cube <- crtabs(fmla, data=.data)
        unweighted_n <-  map_df(dots[unweighted], ~cube@arrays$.unweighted_counts)
        out <- cube %>% 
            as_tibble() %>% 
            select(-.data$row_count) %>% 
            bind_cols(unweighted_n)
    } 
    
    # some cubes don't have an "is_missing" column, so we need the 
    # intersection to handle cubes whether or not they have the column
    names <- intersect(
        c(as.character(groups(.data)), "is_missing", names(dots)),
        names(out)
    )

    # sort the return columns based on the request order
    out <- out[, names]
    return(out)
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
