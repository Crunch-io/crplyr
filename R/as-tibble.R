#' @importFrom tibble as_tibble
as_tibble.CrunchCube <- function (x, ...) {
    ## 1) expand.grid the dimnames
    ## TODO: NA behavior? Add way to get missings not missing. (This uses
    ## as.array to remove extraneous dims, and doing so applies the '@useNA'
    ## from the cube)
    ## TODO: deal with naming for array (and NUMR) variable dims (var.categories?)
    dims <- do.call(expand.grid, dimnames(as.array(x)))
    ## 2) c(bind) the measures
    ## TODO: better access methods for secondary measures in Cube
    ## TODO: if weighted query, include ".unweighted_counts"
    measure_names <- setdiff(names(x@arrays), ".unweighted_counts")
    vals <- sapply(measure_names, function (m) {
        as.vector(crunch:::cubeToArray(x, m))
    }, simplify=FALSE)
    return(as_tibble(c(dims, vals)))
}
