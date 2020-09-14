calculate_steps <- function(var_df, ...) {
  steps <- transmute(var_df, ...)
  
  lapply(names(steps), function(step_alias) {
    out <- steps[[step_alias]][[1]]
    out$data <- modifyList(out$data, list(alias = step_alias))
    out
  })
}

make_query_text <- function(x) {
  steps <- lapply(x@steps, format_ca_auto)
  if (length(steps) == 0) return(NULL)  
  paste(steps, collapse = "\n\n")
}


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
show_query.AutoReadyCrunchDataset <- function(x, ...) {
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

crunch_auto_cmd <- function(formatter, ...) {
  out <- list(formatter = formatter, data = list(...))
  class(out) <- c("crunch_auto_cmd", class(out))
  out
}

format_ca_auto <- function(x, ...) {
  x$formatter(x$data)
}

#' Use Argument Values Baed on Input Variables
#' 
#' Many `crplyr` commands have arguments that allow the use of 
#' formulas (using the compact lambda style functions using `~` or functions, 
#' as in the `purrr` package), where the function will be given a `data.frame`
#' containing the selected Crunch variables to operate on. 
#' @name crplyr-formula-notation 
#' @examples 
#' \dontrun{
#' ds <- ds %>%
#'     mutate(
#'        taste = categorical_array(
#'            taste_a, taste_b, 
#'            # Labels will be Title Case form of subvariable titles
#'            labels = ~stringr::str_to_title(title(.)), 
#'            title = "Taste Rating"
#'        )
#'     )
#' }
NULL

ca_process_formula <- function(x, data) {
  if (rlang::is_function(x) || rlang::is_formula(x)) {
    f <- rlang::as_function(x)
    x <- f(data)
  } 
  x
}
