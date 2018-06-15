#' Flatten a Crunch Cube
#'
#' Crunch Cubes can be expressed as a long data frame instead of a
#' multidimensional array. In this form each dimension of the cube is a variable
#' and the cube values are expressed as columns for each measure. This is useful
#' both to better understand what each entry of a cube represents, and to work
#' with the cube result using tidyverse tools.
#'
#' @param x a CrunchCube
#' @param ... further arguments passed on to `tibble::as_tibble()`
#'
#' @export
#' @importFrom tibble as_tibble
#' @importFrom crunch getDimTypes
#' @importFrom dplyr bind_cols
#' @importFrom purrr map2 map reduce
#' @importFrom stringr str_extract
as_tibble.CrunchCube <- function (x, ...) {
    ## TODO: Consider using `dplyr::tbl_cube` class

    dnames <- dimnames(x@arrays$.unweighted_counts)
    measures <- names(x@arrays)

    # Cubes can have multiple measures, which are represented as their own column
    # in the tibble.
    measure_vals <- sapply(measures, function(y) {
        as.vector(x@arrays[[y]])
    }, simplify = FALSE)

    names(measure_vals)[names(measure_vals) == ".unweighted_counts"] <- "row_count"

    # If there are dimnames, expand.grid and bind them. We also change the names
    # of the two array dimensions to avoid duplicated variable names in  the
    # tibble.
    if (!is.null(dnames)) {
        types <- getDimTypes(x)

        # Change MR selection vars to T/F/NA
        is_selected <- types == "mr_selections"
        dnames <- map2(dnames, is_selected, ~{
            if (.y) {
                return(c(TRUE, FALSE, NA))
            } else {
                return (.x)
            }
        })
        suffixes <- str_extract(types, "_.*$")
        is_array_var <- !is.na(suffixes)
        names(dnames)[is_array_var] <- paste0(names(dnames)[is_array_var], suffixes[is_array_var])
        out <- do.call(expand.grid, dnames)
        names(out) <- add_duplicate_suffix(names(out))

        # Identify which elements of cube represent missing values

        lgl_df <- x@dims %>%
            map("missing") %>%
            expand.grid()

        # apply converts dataframes to matrixes which can be dangerous. In this
        # case it's fine because the data is all logical, but it's good to make
        # that explicit.
        out$is_missing <- apply(as.matrix(lgl_df), 1, any)
        out <- bind_cols(out, measure_vals)
    } else {
        # scalar values, which means no group_by
        out <- bind_cols(measure_vals)
    }
    return(as_tibble(out, ... ))
}


#' @importFrom purrr walk
add_duplicate_suffix <- function(names, sep = "_"){
    walk(unique(names), ~{
        dupes <- names == .
        if (sum(dupes) != 1) {
            names[dupes] <<- paste0(names[dupes], sep, 1:sum(dupes))
        }
    })
    return(names)
}
