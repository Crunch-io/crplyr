#' Modify Variables in a Crunch Automation Script
#'
#' Adds commands to a Crunch Automation script that modify the contents
#' of variables. Unlike `dplyr::mutate` on a traditional R `data.frame`,
#' the variables are not created until you run the command [`compute()`].
#' 
#' @param .data A crunch Dataset
#' @param ... Arguments that create variables using the [`create-vars`] functions.
#' @name mutate
#' @family automation script commands
#' @export
#' @importFrom dplyr mutate
#' @examples
#' \dontrun{
#' ds <- ds %>%
#'     mutate(
#'        taste = categorical_array(
#'            taste_a, taste_b, 
#'            labels = c("Brand A", "Brand B"), 
#'            title = "Taste Rating"
#'        )
#'     )
#'     
#' # Show the query
#' show_query(ds)
#' 
#' # Execute the query 
#' ds <- ds %>% compute()
#' }
mutate.CrunchDataset <- function(.data, ...) {
  if (!inherits(.data, "AutomationCrunchDataset")) .data <- as_crunch_auto_tbl(.data)
  
  out <- .data
  out@steps <- c(out@steps, calculate_steps(out@var_tibble, ...))
  out
}




#' Functions for Creating Variables Inside `mutate()`
#' 
#' When creating and modifying variables in a `CrunchDataset` with `mutate()`, 
#' they must be created using the functions listed below.
#'
#' @name create-vars
#' @family creating variables functions
NULL


#' Create a categorical array
#'
#' Create a categorical array from one or more existing variables.
#' Based on the Crunch Automation command 
#' [CREATE CATEGORICAL ARRAY](https://help.crunch.io/hc/en-us/articles/360042039392).
#' 
#' If `...` is specified using aliases or `dplyr::across()`, then
#' the other arguments can take functions based on the variables
#' selected using [`crplyr-formula-notation`]. 
#'
#' @param ... One or more crunch variables selected using their alias,
#' [`dplyr::across()`], or the special selection functions in [`ca`].
#' @param labels The labels for the subvariables (also called the subvariable 
#' names in the crunch R package).
#' @param title Optional, the title of the variables created (also called the variable name
#' in the crunch R package)
#' @param description Optional, the description of the variables created
#' @param notes Optional, the notes of the variable created
#' @family creating variables functions
#' @export
categorical_array <- function(
  ..., 
  labels = NULL, 
  title = NULL, 
  description = NULL, 
  notes = NULL
) {
  .vars <- as_crunch_var_df(list(...))
  sv_aliases <- internal_aliases(.vars)
  
  labels <- ca_process_formula(labels, .vars)
  title <- ca_process_formula(title, .vars)
  description <- ca_process_formula(description, .vars)
  notes <- ca_process_formula(notes, .vars)
  
  cmd <- crunch_auto_cmd(
    ca_template(
      "CREATE CATEGORICAL ARRAY\n", 
      "  {crplyr:::ca_comma_separated(sv_aliases)}\n",
      "  LABELS {crplyr:::ca_comma_separated(labels)}\n",
      "AS {alias}",
      "{crplyr:::ca_optional('TITLE', title)}",
      "{crplyr:::ca_optional('DESCRIPTION', description)}",
      "{crplyr:::ca_optional('NOTES', notes)}", 
      ";"
    ),
    sv_aliases = sv_aliases,
    labels = labels,
    title = title,
    description = description,
    notes = notes
  )
  
  return(list(cmd))
}


#' Convert a Variable's Type
#' 
#' When called from [`mutate`], converts the variables type either as a new variable, or in place 
#' (in which case it is considered a "schema command" and must be run before any
#' "non-schema commands").
#' Based on the Crunch Automation commands 
#' [CONVERT](https://help.crunch.io/hc/en-us/articles/360042247191-CONVERT-command) and 
#' [CREATE CONVERT](https://help.crunch.io/hc/en-us/articles/360047136371-CREATE-CONVERT-command).
#' 
#' If `...` is specified using aliases or `dplyr::across()`, then
#' the other arguments can take functions based on the variables
#' selected using [`crplyr-formula-notation`]. 
#'
#' @inheritParams categorical_array
#' @param new_aliases If only one variable is specified, this argument is ignored,
#' the alias comes from the name of the argument in mutate (and if it's the same
#' the variable will be modified in place, while if different it will create a new
#' variable). If more than one variable is specified `NULL`, the default, will modify
#' the variables in place, or a vector of aliases for the new variables (or a 
#' [`crunch-formula-notation`] that creates one). 
#'
#' @family creating variables functions
#' @export
convert_to_text <- function(
  ..., 
  new_aliases = NULL,
  title = NULL,
  description = NULL,
  notes = NULL
) {
  .vars <- as_crunch_var_df(list(...))
  vars_have_ca_expansion <- !all(map_lgl(.vars, is.variable))
  old_aliases <- internal_aliases(.vars)
  
  new_aliases <- ca_process_formula(new_aliases, .vars)
  title <- ca_process_formula(title, .vars)
  description <- ca_process_formula(description, .vars)
  notes <- ca_process_formula(notes, .vars)
  
  cmd <- crunch_auto_cmd(
    function(x) {
      if (is.null(x$new_aliases)) {
        if (length(x$old_aliases) == 1 && !vars_have_ca_expansion) {
          x$new_aliases <- list(noquote(x$alias)) 
        } else {
          x$new_aliases <- x$old_aliases
        }
      } else {
        x$new_aliases <- map(x$new_aliases, noquote)
      }
      
      if (identical(x$old_aliases, x$new_aliases)) {
        template <- ca_template(
          "CONVERT {crplyr:::ca_comma_separated(old_aliases)} TO TEXT;"
        )
      } else {
        template <- ca_template(
          "CREATE CONVERT\n", 
          "  {crplyr:::ca_comma_separated(old_aliases)}\n",
          "  TO TEXT\n",
          "AS {crplyr:::ca_comma_separated(new_aliases)}",
          "{crplyr:::ca_optional('TITLE', title)}",
          "{crplyr:::ca_optional('DESCRIPTION', description)}",
          "{crplyr:::ca_optional('NOTES', notes)}",
          ";"
        )
      }
      template(x)
    },
    old_aliases = old_aliases,
    new_aliases = new_aliases,
    title = title,
    description = description,
    notes = notes,
    vars_have_ca_expansion = vars_have_ca_expansion
  )
  
  return(list(cmd))
}
