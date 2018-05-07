#' Flatten a Crunch Cube
#'
#' Crunch Cubes can be expressed as a long data frame instead of a
#' multidimensional array. In this form each dimension of the cube is a variable
#' of the dataframe and the cube values are expressed columns for each measure.
#' This is useful both to better understand what each entry of a cube
#' represents, and to work with the cube result using tidyverse tools.
#'
#' @param x a CrunchCube
#' @param return_real Do you want to return the tibble represenatation of the
#'   real cube with MR selection dimensions and missing values. Mostly useful
#'   for debugging complex cubes.
#' @param ... futher arguments passed on to `dplyr::as_tibble()`
#'
#' @export
#' @importFrom tibble as_tibble
as_tibble.CrunchCube <- function (x, return_real = FALSE, ...) {
    ## TODO: Consider using `dplyr::tbl_cube` class
    ## TODO: NA behavior? Add way to get missings not missing. (This uses
    ## as.array to remove extraneous dims, and doing so applies the '@useNA'
    ## from the cube)   
    ## TODO: better access methods for secondary measures in Cube
    ## TODO: if weighted query, include ".unweighted_counts"
    if (return_real) {
        dnames <- dimnames(x@arrays$count)
        # Cubes can have multiple measures, which are represented as their own column
        # in the tibble. 
        measure_vals <- sapply(names(x@arrays), function(y) {
            as.vector(x@arrays[[y]])
        }, simplify = FALSE)
    } else {
        measure_names <- setdiff(names(x@arrays), ".unweighted_counts")
        measure_vals <- sapply(measure_names, function (m) {
            as.vector(crunch:::cubeToArray(x, m))
        }, simplify = FALSE)

        dnames <- dimnames(as.array(x))
        
    }
    # If there are dimnames, expand.grid and c(bind) them. We also 
    # change the names of the two array dimensions to avoid duplicated variable names
    if (!is.null(dnames)) {
        types <- getDimType(x@dims)
        if (!return_real) {
            # mr_selection dimension is suppressed from the user cube, so we 
            # need to suppress thos from the returned types. 
            types <- types[types != "mr_selections"]
        }
        suffixes <- stringr::str_extract(types, "_.*$")
        is_array_var <- !is.na(suffixes)
        names(dnames)[is_array_var] <- paste0(names(dnames)[is_array_var], suffixes[is_array_var])
        out <- do.call(expand.grid, dnames)
        out <- bind_cols(out, measure_vals)
    } else {
        # scalar values, which means no group_by
        out <- bind_cols(measure_vals)
    }
    return(as_tibble(out, ... ))
}


#' Get dimension type
#'
#' This function returns the specific type of each cube dimension. This is useful
#' when cubes contain categorical array or multiple response variables because it
#' identifies the dimensions of the cube which refer to the differ parts of
#' array variable:
#' - `ca_items`: Categorical array items
#' - `ca_categories`: The categories of the categorical array
#' - `mr_items`: Multiple response options or items
#' - `mr_selections`: The selection status for a multiple response variable
#'
#' @return A character vector. This is identical to `types()` except that
#' the array variable types are more specific.
#' @param x a CrunchCube or CubeDims object
#'
#' @return a character vector of dimension types
#' @export
#' @keywords internal
getDimType <-  function (x) {
    #TODO Remove this when the function is included in rcrunch. 
    vars <- crunch::variables(x)
    out <- crunch::types(vars)
    out[crunch:::is.selectedDimension(x)] <- "mr_selections"
    for (i in seq_along(vars)) {
        if (i == length(vars)) {
            break
        }
        if (out[i + 1] == "mr_selections") {
            out[i] <- "mr_items"
        } else if (out[i] == "subvariable_items") {
            out[i] <- "ca_items"
            out[i + 1] <- "ca_categories"
        }
    }
    names(out) <- names(dimnames(x))
    return(out)
}
