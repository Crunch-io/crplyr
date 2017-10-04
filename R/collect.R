
#' Collect a crunch dataset from the server
#'
#' This function brings a crunch dataset into memory so that you can
#' work with the data using R functions. Since this can create a long running
#' query it is recommended to try to filter the dataset down as much as possible
#' before running `collect()`.
#' @param ds A crunch Dataset
#'
#' @return a tibble
#' @export
#' @importFrom tibble as_data_frame
#'
#' @examples
#' \dontrun{
#' ds %>% 
#'    select(cyl, gear) %>% 
#'    collect()
#' }
collect <- function(ds) {
    out <- lapply(ds, as.vector)
    names(out) <- names(ds)
    return(as_data_frame(out))
}