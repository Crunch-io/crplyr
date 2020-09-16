#' Create a categorical array
#'
#' Create a categorical array from one or more existing variables.
#' The new variable's alias is based on the name of the argument within
#' `mutate()`. Based on the Crunch Automation command 
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
  .dots <- prepare_nested_cmds(list(...))
  
  .vars <- crunch_var_df_from_dots(.dots)
  sv_aliases <- internal_aliases(.vars)
  
  labels <- ca_process_formula(labels, .vars)
  title <- ca_process_formula(title, .vars)
  description <- ca_process_formula(description, .vars)
  notes <- ca_process_formula(notes, .vars)
  
  cmd <- crunch_auto_cmd(
    formatter = ca_template(
      "CREATE CATEGORICAL ARRAY\n", 
      "  {crplyr:::ca_comma_separated(sv_aliases)}\n",
      "  LABELS {crplyr:::ca_comma_separated(labels)}\n",
      "AS {alias}",
      "{crplyr:::ca_optional('TITLE', title)}",
      "{crplyr:::ca_optional('DESCRIPTION', description)}",
      "{crplyr:::ca_optional('NOTES', notes)}", 
      ";"
    ),
    get_aliases = function(x) x$alias,
    sv_aliases = sv_aliases,
    labels = labels,
    title = title,
    description = description,
    notes = notes
  )
  
  nest_cmds(cmd, .dots)
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
  .dots <- prepare_nested_cmds(list(...))
  
  .vars <- crunch_var_df_from_dots(.dots)
  vars_have_ca_expansion <- !all(map_lgl(.vars, is_var_like))
  old_aliases <- internal_aliases(.vars)
  
  new_aliases <- ca_process_formula(new_aliases, .vars)
  title <- ca_process_formula(title, .vars)
  description <- ca_process_formula(description, .vars)
  notes <- ca_process_formula(notes, .vars)
  
  cmd <- crunch_auto_cmd(
    formatter = function(x) {
      if (is.null(x$new_aliases)) {
        if (length(x$old_aliases) == 1 && !x$vars_have_ca_expansion) {
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
    get_aliases = function(x) {
      if (!is.null(x$new_aliases)) {
        as.character(x$new_aliases)
      } else if (x$vars_have_ca_expansion) {
        NULL # Can't guess variables have crunch expansion
      } else if (length(x$old_aliases) == 1) {
        x$alias 
      } else {
        as.character(x$old_aliases)
      }
    },
    old_aliases = old_aliases,
    new_aliases = new_aliases,
    title = title,
    description = description,
    notes = notes,
    vars_have_ca_expansion = vars_have_ca_expansion
  )
  
  nest_cmds(cmd, .dots)
}
