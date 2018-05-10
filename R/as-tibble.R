#' Flatten a Crunch Cube
#'
#' Crunch Cubes can be expressed as a long data frame instead of a
#' multidimensional array. In this form each dimension of the cube is a variable
#' and the cube values are expressed columns for each measure. This is useful
#' both to better understand what each entry of a cube represents, and to work
#' with the cube result using tidyverse tools.
#'
#' @param x a CrunchCube
#' @param show_metadata By default this function returns additional metatada
#'   about the cube. In particular the unweighted counts for each entry, and
#'   whether that entry represents a missing value. If `FALSE` this metadata
#'   will be suppressed.
#' @param ... futher arguments passed on to `tibble::as_tibble()`
#'
#' @export
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @importFrom purrr map2 map reduce
#' @importFrom stringr str_extract
as_tibble.CrunchCube <- function (x, show_metadata = TRUE, ...) {
    ## TODO: Consider using `dplyr::tbl_cube` class
    dnames <- dimnames(x@arrays$count)
    measures <- names(x@arrays)
    if (!show_metadata) {
        # we need this in part to hide unweighted counts from summarize
        measures <- setdiff(measures, ".unweighted_counts")
    }
    
    # Cubes can have multiple measures, which are represented as their own column
    # in the tibble. 
    measure_vals <- sapply(measures, function(y) {
        as.vector(x@arrays[[y]])
    }, simplify = FALSE)
    
    # If there are dimnames, expand.grid and bind them. We also change the names
    # of the two array dimensions to avoid duplicated variable names in  the
    # tibble.
    if (!is.null(dnames)) {
        # Change MR selection vars to T/F/NA
        is_selected <- is.selectedDimension(x@dims)
        dnames <- map2(dnames, is_selected, ~{
            if (.y) {
                return(c(TRUE, FALSE, NA))
            } else {
                return (.x)
            }
        })
        
        types <- getDimType(x@dims)
        suffixes <- str_extract(types, "_.*$")
        is_array_var <- !is.na(suffixes)
        names(dnames)[is_array_var] <- paste0(names(dnames)[is_array_var], suffixes[is_array_var])
        out <- do.call(expand.grid, dnames)
        out <- bind_cols(out, measure_vals)
        
        # identify which elements of cube represent missing values
        if (show_metadata) {
            out$is_missing <- x@dims %>% 
                map("missing") %>% 
                expand.grid() %>% 
                reduce(`|`)
        }
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
#' - `mr_selections`: The selection status for each item
#' @param x a CrunchCube or CubeDims object
#' @return A character vector. This is identical to `types()` except that
#' the array variable types are more specific.
#' @keywords internal
#' @importFrom crunch variables types
getDimType <-  function (x) {
    #TODO Remove this when the function is included in rcrunch. 
    vars <- variables(x)
    out <- types(vars)
    out[is.selectedDimension(x)] <- "mr_selections"
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


#' Check if a dimension is an MR selection dimension
#'
#' @param dims A Crunch Cube Dimension
#'
#' @return A logical vector
#' @keywords internal
#' @importFrom crunch variables aliases types index
is.selectedDimension <- function (dims) {
    #TODO removed when exported from rcrunch
    is.it <- function (x, dim, MRaliases) {
        x$alias %in% MRaliases &&
            x$type == "categorical" &&
            length(dim$name) == 3 &&
            dim$name[1] == "Selected"
    }
    vars <- variables(dims)
    # We only need to check if the categories are the magical Selected
    # categories if there is an MR somewhere with the same alias
    MRaliases <- aliases(vars)[types(vars) == "subvariable_items"]
    
    # determine which dimensions are selected MR dimensions
    selecteds <- mapply(is.it, x = index(vars), dim=dims@.Data,
                        MoreArgs = list(MRaliases=MRaliases))
    names(selecteds) <- dims@names
    return(selecteds)
}