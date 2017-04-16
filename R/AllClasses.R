#' A Crunch Dataset "Grouped By" Something
#'
#' This is a subclass of `CrunchDataset` that has a field for recording
#' "group_by" expressions.
#'
#' @export
#' @importFrom methods new
GroupedCrunchDataset <- setClass("GroupedCrunchDataset",
    contains="CrunchDataset",
    slots=c(groupBy="list"))
