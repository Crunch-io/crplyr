#' Create or Modify Variables in a Crunch Dataset
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
  results <- run_steps(out@var_df, ...)
  out@steps <- c(out@steps, results$steps)
  out@var_df <- results$var_df
  out
}

# Useful for testing mutate
test_create_single_var_cmd <- function(..., arg = NULL) {
  .dots <- prepare_nested_cmds(list(...))
  
  .vars <- crunch_var_df_from_dots(.dots)
  in_aliases <- internal_aliases(.vars)
  
  arg <- ca_process_formula(arg, .vars)

  cmd <- crunch_auto_cmd(
    formatter = ca_template(
      "{crplyr:::ca_list_to_text(items = in_aliases, sep_newline = FALSE, start_newline = FALSE)} AS {alias}", #nolint
      "{crplyr:::ca_list_to_text('ARG', items = arg, indent = 1, sep_newline = FALSE, start_newline = FALSE)};", #nolint
    ),
    get_aliases = function(x) x$alias,
    in_aliases = in_aliases,
    arg = arg
  )
  
  nest_cmds(cmd, .dots)
}

# Another test helper, making sure we allow steps to create multiple variables
test_create_multi_var_cmd <- function(..., new_aliases) {
  .dots <- prepare_nested_cmds(list(...))
  
  .vars <- crunch_var_df_from_dots(.dots)
  in_aliases <- internal_aliases(.vars)
  
  new_aliases <- ca_process_formula(new_aliases, .vars)
  new_aliases <- map(new_aliases, noquote)
  
  cmd <- crunch_auto_cmd(
    formatter = ca_template(
      "{crplyr:::ca_list_to_text(items = in_aliases, sep_newline = FALSE, start_newline = FALSE)} ", #nolint
      "{crplyr:::ca_list_to_text('AS', items = new_aliases, sep_newline = FALSE, start_newline = FALSE)};" #nolint
    ),
    get_aliases = function(x) as.character(x$new_aliases),
    in_aliases = in_aliases,
    new_aliases = new_aliases
  )
  
  nest_cmds(cmd, .dots)
}


#' Functions for Creating Variables Inside `mutate()`
#' 
#' When creating and modifying variables in a `CrunchDataset` with `mutate()`, 
#' they must be created using the functions listed below.
#'
#' @name create-vars
#' @family creating variables functions
NULL
