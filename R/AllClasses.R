#' A Crunch Dataset "Grouped By" Something
#'
#' This is a subclass of `crunch::CrunchDataset` that has a field for recording
#' "group_by" expressions.
#'
#' @export
#' @importFrom methods new
#' @importClassesFrom crunch CrunchDataset
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


setOldClass("crunch_var_df")

#' A Crunch Dataset With Crunch Automation Commands "At The Ready"
#'
#' This is a subclass of `crunch::CrunchDataset` that has a field for recording
#' Crunch Automation commands that will get run when `collect` or `calculate`
#' is called. 
#'
#' @export
AutoReadyCrunchDataset <- setClass(
  "AutoReadyCrunchDataset",
  contains = "CrunchDataset",
  slots = c(
    steps = "list",
    var_tibble = "crunch_var_df"
  )
)
