
#' Collect a crunch dataset from the server
#'
#' This function brings a crunch dataset into memory so that you can
#' work with the data using R functions. Since this can create a long running
#' query it is recommended that you try to filter the dataset down as much as possible
#' before running `collect()`.
#' @param x A crunch Dataset
#' @param ... Other arguments passed on to [tibble::as_data_frame()] 
#'
#' @return a tibble
#' @export
#' @importFrom tibble as_data_frame
#' @importFrom dplyr collect
#'
#' @examples
#' \dontrun{
#' ds %>% 
#'    select(cyl, gear) %>% 
#'    collect()
#' }
collect.CrunchDataset <- function(x, ...) {
    out <- lapply(x, as.vector)
    names(out) <- names(x)
    return(as_data_frame(out, ...))
}