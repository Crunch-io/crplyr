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
        types <- crunch::getDimTypes(x)
        names(types) <- names(dnames)

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
        names(dnames)[is_array_var] <- paste0(
            names(dnames)[is_array_var],
            suffixes[is_array_var]
            )
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
    out <- as_tibble(out, ...)
    meta <- map(x@dims, "references") %>%
        map(~.[names(.) != "categories"])

    meta <- c(meta, rep(NA, length(measure_vals) + 1)) # the '1' is for the is_missing column
    names(meta) <- names(out)
    attr(out, "cube_metadata")  <- meta

    types <- c(types, "missing", rep("measure", length(measure_vals)))
    names(types) <- names(out)
    attr(out, "types") <- types
    attr(out, "useNA") <- x@useNA
    class(out) <- c("tbl_crunch_cube", "tbl_df", "tbl", "data.frame")
    return(out)
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

as_tibble.tbl_crunch_cube <- function(x, ...){
    attr(x, "types") <- NULL
    attr(x, "cube_metadata") <- NULL
    attr(x, "useNA") <- NULL
    class(x) <- c("tbl_df", "tbl", "data.frame")
    return(as_tibble(x, ...))
}

dim_types <- function(x) {
    stopifnot(inherits(x, "tbl_crunch_cube"))
    return(attr(x, "types"))
}

is_dimension <- function(x) {
    return(
        dim_types(x) != "missing" & dim_types(x) != "measure"
    )
}

#' @importFrom purrr map_lgl
cube_attribute <- function(x, attr = "all"){
    stopifnot(inherits(x, "tbl_crunch_cube"))
    metadata <- attr(x, "cube_metadata")
    if (attr == "all") {
        return(metadata)
    }
    out <- map(metadata, attr)
    out[map_lgl(out, is.null)] <- NA
    return(unlist(out))
}

`[.tbl_crunch_cube` <- function(x, i, j, drop = FALSE) {
    # TODO see if there's a way to subset the tibble directly without reassigning
    # the attributes.
    out <- as_tibble(x)[i, j, drop]
    class(out) <- class(x)
    attr(out, "cube_metadata") <- attr(x, "cube_metadata")[j]
    attr(out, "types") <- attr(x, "types")[j]
    attr(out, "useNA") <- attr(x, "useNA")
    return(out)
}

`[[.tbl_crunch_cube` <- function(x, i, j) {
    if (missing(j)) {
        if (length(i) == 1) {
            return(as_tibble(x)[[i]])
        }
        return(x[, i])
    }
}


as_tibble.CrunchCubeCalculation <- function(x){
    dnames <- dimnames(x)
    types <- crunch::getDimTypes(attr(x, "dims"))
    names(types) <- names(dnames)
    # _items, _categories, and _selections, indicate that this dimension is part
    # of an array. In order to not duplicate names, we add the suffix back to
    # the name if it exists.
    suffixes <- str_extract(types, "_.*$")
    is_array_var <- !is.na(suffixes)
    names(dnames)[is_array_var] <- paste0(
        names(dnames)[is_array_var],
        suffixes[is_array_var]
    )
    out <- expand.grid(dnames)

    meta <- map(attr(x, "dims"), "references") %>%
        map(~.[names(.) != "categories"])

    meta <- c(meta, NA)
    calc_type <- attr(x, "type")
    out[[calc_type]] <- as.vector(x)

    attr(out, "types") <- structure(
        c(types, "measure"),
        names = c(names(types), calc_type)
    )
    attr(out, "cube_metadata") <- meta
    class(out) <- c("tbl_crunch_cube", "tbl_df", "tbl", "data.frame")
    return(out)
}
