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
    return(as.data.frame(x, force = TRUE, ...))
}

#' @importFrom dplyr group_by collect %>%
#' @importFrom rlang !!! syms
#' @name collect
#' @export
collect.GroupedCrunchDataset <- function(x, ...){
    out <- transfer_groups(collect.CrunchDataset(x, ...), x)
    return(out)
}

# Broken out for testing
transfer_groups <- function(df, grouped_df) {
    return(group_by(df, !!!syms(group_vars(grouped_df))))
}

