#' Collect a crunch dataset from the server
#'
#' This function brings a Crunch dataset into memory so that you can
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
#' @importFrom tibble data_frame as_data_frame
#' @importFrom dplyr collect bind_cols
#' @importFrom rlang !! :=
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
    ## Calling as.vector on an array variable returns a dataframe
    ## this is necessary in order to harmonize the array variable output
    ## with the non-array variable output.
    ## TODO replace with purrr::map_if if available.
    list_to_df <- function (entry, name) {
        if (!is.data.frame(entry)) {
            entry <- data_frame(!!name := entry)
        }
        return(entry)
    }
    out <- mapply(list_to_df, out, names(x), SIMPLIFY = FALSE) %>%
        bind_cols()
    return(as_data_frame(out, ...))
}

#' @importFrom dplyr group_by collect %>%
#' @importFrom rlang !!! syms
#' @name collect
#' @export
collect.GroupedCrunchDataset <- function(x, ...){
    out <- collect.CrunchDataset(x, ...) %>%
        group_by(!!!syms(group_vars(x)))
    return(out)
}
