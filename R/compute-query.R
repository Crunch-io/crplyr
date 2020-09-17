#' Show Text of Crunch Automation Query
#' 
#' Show the text of the Crunch Automation script that you've prepared 
#' using crplyr. To execute the query, see [`compute()`].
#'
#' @param x An `AutomationCrunchDataset` (a `CrunchDataset` modified with [`mutate()`] or similar)
#' @param ... ignored
#'
#' @return invisibly, the text of the Crunch Automation Script
#' @name show_query
#' @export
#' @family automation script commands
#' @importFrom dplyr show_query
#' @examples
#' \dontrun{
#' ds %>%
#'     mutate(
#'        taste = categorical_array(
#'            taste_a, taste_b, 
#'            labels = c("Brand A", "Brand B"), 
#'            title = "Taste Rating"
#'        )
#'     ) %>%
#'     show_query()
#' }
show_query.AutomationCrunchDataset <- function(x, ...) {
  out <- make_query_text(x)
  cat("---Crunch Automation command---\n")
  cat(out)
  invisible(out)
}

#' Run Crunch Automation Query
#' 
#' Run the commands that have been created by crplyr's [`mutate()`] and
#' similar commands.  
#'
#' @param x An `AutomationCrunchDataset` (a `CrunchDataset` modified with [`mutate()`] or similar)
#' @param name ignored
#' @param ... ignored
#'
#' @return invisibly, the CrunchDataset after running the command
#' @export
#' @importFrom dplyr compute
#' @importFrom crunch runCrunchAutomation
#' @name compute
#' @family automation script commands
#' @examples
#' \dontrun{
#' ds <- ds %>%
#'     mutate(
#'        taste = categorical_array(
#'            taste_a, taste_b, 
#'            labels = c("Brand A", "Brand B"), 
#'            title = "Taste Rating"
#'        )
#'     ) %>%
#'     compute()
#' }
compute.AutomationCrunchDataset <- function(x, name = NULL, ...) {
  query <- make_query_text(x)
  if (is.null(query)) return(x)
  
  out <- runCrunchAutomation(x, query)
  CrunchDataset(out)
}

make_query_text <- function(x) {
  commands <- lapply(x@commands, format)
  if (length(commands) == 0) return(NULL)  
  paste(commands, collapse = "\n\n")
}
