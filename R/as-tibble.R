#' @export
#' @importFrom tibble as_tibble
as_tibble.CrunchCube <- function (x, ...) {
    ## TODO: Consider using `dplyr::tbl_cube` class
    ## TODO: NA behavior? Add way to get missings not missing. (This uses
    ## as.array to remove extraneous dims, and doing so applies the '@useNA'
    ## from the cube)
    ## TODO: deal with naming for array (and NUMR) variable dims (var.categories?)

    ## 1) get the measures
    ## TODO: better access methods for secondary measures in Cube
    ## TODO: if weighted query, include ".unweighted_counts"
    measure_names <- setdiff(names(x@arrays), ".unweighted_counts")
    vals <- sapply(measure_names, function (m) {
        as.vector(crunch:::cubeToArray(x, m))
    }, simplify=FALSE)

    ## 2) If there are dimnames, expand.grid and c(bind) them
    dnames <- dimnames(as.array(x))
    ## NULL if scalar value, which means no "group_by"
    if (!is.null(dnames)) {
        dims <- do.call(expand.grid, dnames)
        vals <- c(dims, vals)
    }
    return(as_tibble(vals))
}
