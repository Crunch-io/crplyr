
#' Collect a crunch dataset from the server
#'
#' This function brings a crunch dataset into memory so that you can
#' work with the data using R functions. Since this can create a long running
#' query it is recommended that you try to filter the dataset down as much as possible
#' before running `collect()`. When collecting a grouped CrunchDataset, 
#' the grouping will be preserved. 
#'
#' @param x A crunch Dataset
#' @param ... Other arguments passed on to [tibble::as_data_frame()] 
#'
#' @return a tbl_df or grouped_df
#' @export
#' @importFrom tibble as_data_frame
#' @importFrom dplyr collect
#' @name collect
#'
#' @examples
#' \dontrun{
#' ds %>% 
#'    group_by(cyl) %>% 
#'    select(cyl, gear) %>% 
#'    collect()
#' }
    collect.CrunchDataset <- function(x, ...) {
        out <- lapply(x, as.vector)
        names(out) <- names(x)
        return(as_data_frame(out, ...))
    }

#' @importFrom dplyr group_by collect
#' @importFrom rlang !!! syms
#' @importFrom magrittr %>%
#' @name collect
#' @export
collect.GroupedCrunchDataset <- function(x, ...){
    out <- lapply(x, as.vector)
    names(out) <- names(x)
    out <- as_data_frame(out, ...) %>% 
        group_by(!!!syms(group_vars(x)))
    return(out)
}
