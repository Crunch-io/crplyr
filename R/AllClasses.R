#' A Crunch Dataset "Grouped By" Something
#'
#' This is a subclass of `CrunchDataset` that has a field for recording
#' "group_by" expressions.
#'
#' @export
#' @importFrom methods new
#' @examples
#' \dontrun{
#' ds <- loadDataset("Your dataset name")
#' class(ds) ## "CrunchDataset"
#' grouped_ds <- group_by(ds, var1)
#' class(grouped_ds) ## "GroupedCrunchDataset"
#' }
GroupedCrunchDataset <- setClass("GroupedCrunchDataset",
    contains="CrunchDataset",
    slots=c(groupBy="list"))
