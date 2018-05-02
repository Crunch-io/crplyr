#' Flatten a Crunch Cube
#'
#' Crunch Cubes can be expressed as a long data frame instead of a
#' multidimensional array. In this form each dimension of the cube is a variable
#' of the dataframe and the cube values is expressed columns for each measure.
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
    ## TODO: deal with naming for array (and NUMR) variable dims (var.categories?)
    
    ## 1) get the measures
    ## TODO: better access methods for secondary measures in Cube
    ## TODO: if weighted query, include ".unweighted_counts"
    measure_names <- setdiff(names(x@arrays), ".unweighted_counts")
    if (return_real) {
        is_selected <- crunch:::is.selectedDimension(x@dims)
        dnames <- dimnames(x@arrays$count)
        out <- do.call(expand.grid, dnames)
        if (any(is_selected)) {
            names(out)[is_selected] <- paste0(names(out)[is_selected], "_selections")
        }
        # Cubes can have multiple measures, which are represented as their own column
        # in the tibble. 
        measure_vals <- lapply(measure_names, function(y) {
            as.vector(x@arrays[[y]])
        })
        names(measure_vals) <- measure_names
        out <- bind_cols(out, measure_vals)
    } else {
        out <- sapply(measure_names, function (m) {
            as.vector(crunch:::cubeToArray(x, m))
        }, simplify = FALSE)
        
        ## 2) If there are dimnames, expand.grid and c(bind) them
        dnames <- dimnames(as.array(x))
        ## NULL if scalar value, which means no "group_by"
        if (!is.null(dnames)) {
            dims <- do.call(expand.grid, dnames)
            out <- c(dims, out)
        }
    }
    return(as_tibble(out, ... ))
}
