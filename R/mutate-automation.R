as_crunch_auto_tbl <- function(x) { # TODO: move to a S4 initializer?
  out <- AutomationCrunchDataset(x)
  out@steps <- list()
  out@var_tibble <- as_crunch_var_df(x)
  out
}

# A data.frame with list columns containing crunch variables
# `tibble` doesn't allow this data.frame because CrunchVars
# are not true "vectors" so be careful of manipulating this
#' @importFrom purrr map map_chr
as_crunch_var_df <- function(vars) { #TODO: use traditional S4 "initialize"?
  if (is.dataset(vars)) {
    all_aliases <- aliases(allVariables(vars))
    var_list <- map(all_aliases, function(alias) vars[[alias]])
    names(var_list) <- all_aliases
  } else {
    # across gives a data.frame 
    # TODO: this may already be what we want, or at least might be with correct subsetting
    # defined on the crunch_var_df class, but need to check
    if (is.data.frame(vars[[1]])) { 
      vars <- purrr::flatten(vars) 
    }
    all_aliases <- map_chr(vars, function(var) if (is.variable(var)) alias(var) else as.character(var))
    var_list <- setNames(vars, all_aliases)
  }
  
  structure(var_list, row.names = c(NA, -1L), class = c("crunch_var_df", "data.frame"))
}



#' Crunch variable metadata for `crunch_var_df` objects
#' 
#' Get variable aliases, titles (also called "variable names" by the 
#' crunch R package), descriptions and notes from the variables 
#' collected into a data.frame when using [crplyr-formula-notation]().
#'
#' @param x A `crunch_var_df` object which contains `CrunchVariable`
#' objects.
#'
#' @return A character vector
#' @name crunch_var_df-meta
NULL

#' @export
#' @rdname crunch_var_df-meta 
setMethod("aliases", "crunch_var_df", function(x) {
  if (!all(map_lgl(x, is.variable))) stop("All variables must be CrunchVariables")
  names(x)
})

# Internally we allow non crunchvars, we want to preserve the
# noquote status of non crunch vars, as well as backtick aliases
# with spaces
internal_aliases <- function(x) {
  map(x, function(var) {
    if (is.variable(var)) {
      out <- alias(var)
      if (grepl("[[:space:]]", out)) out <- paste0("`", out, "`")
      noquote(out)
    } else {
      var
    }
  })
}


#' @export
#' @rdname crunch_var_df-meta 
setMethod("titles", "crunch_var_df", function(x) {
  if (!all(map_lgl(x, is.variable))) stop("All variables must be CrunchVariables")
  map_chr(x, name)
})

#' @export
#' @rdname crunch_var_df-meta 
setMethod("descriptions", "crunch_var_df", function(x) {
  if (!all(map_lgl(x, is.variable))) stop("All variables must be CrunchVariables")
  map_chr(x, description)
})

#' @export
#' @rdname crunch_var_df-meta 
setMethod("notes", "crunch_var_df", function(x) {
  if (!all(map_lgl(x, is.variable))) stop("All variables must be CrunchVariables")
  map_chr(x, notes)
})


calculate_steps <- function(var_df, ...) {
  steps <- transmute(var_df, ...)
  
  lapply(names(steps), function(step_alias) {
    out <- steps[[step_alias]][[1]]
    out$data <- modifyList(out$data, list(alias = step_alias))
    out
  })
}


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

ca_comma_separated <- function(items, newline = FALSE, indent = 0) {
  indent_mark <- paste(rep(" ", indent), collapse = "")
  collapse_mark <- if (newline) paste0("\n", indent_mark) else ", "
  
  items <- ca_quote_items(items)
  
  out <- glue::glue_collapse(
    glue::glue("{items}"),
    sep = collapse_mark
  )
  glue::glue("{indent_mark}{out}")
}

ca_optional <- function(label, items, indent = 0, newline = TRUE) {
  if (is.null(items)) return("")
  
  indent_mark <- paste(rep(" ", indent), collapse = "")
  newline_mark <- if (newline) paste0("\n", indent_mark) else " "
  items <- glue::glue_collapse(ca_quote_items(items), sep = ", ")
  glue::glue(
    "{newline_mark}{label} {items}"
  )
}

ca_quote_items <- function(items) {
  purrr::map_chr(items, function(item) {
    if (is.numeric(item) || inherits(items, "noquote") || inherits(item, "noquote")) {
      paste0(item)
    } else {
      paste0("\"", item, "\"")
    }
  })
}

ca_template <- function(...) {
  function(x) glue::glue_data(x, ...)
}


#' Crunch Automation Syntax Helpers
#' 
#' Functions that help create special keywords for use in Crunch Automation commands.
#' 
#' The `ca` object contains the following objects and functions:
#' - `copy`: Returns `COPY`, used to indicate that metadata should be copied from source
#'       in some circumstances.
#' - `dots(x, y)`: A function that takes two aliases (`x`, `y`) and returns `x...y` which
#'       is shorthand in Crunch Automation for selecting the variables between x & y.
#' - `like(x)`: A function that takes a single string and wraps it in `LIKE("")`, which in
#'       Crunch Automation interprets it similar to "SQL's" LIKE statement, where a 
#'       `%` matches any string and `_` matches a single character.
#' - `regex(x)`/`regexp(x)`: Functions that take a single string in wrap in `REGEX("")`/
#'       `REGEXP("")`, which Crunch Automation interprets.
#' - `use_descriptions`: Returns `USE DESCRIPTIONS` which indicates the metadata should come
#'        from the descriptions in some circumstances.
#' - `use_titles`: Returns `USE TITLES` which indicates the metadata should come
#'        from the titles in some circumstances.
#' - `keyword(x)`: Returns the text unquoted, useful if you want to use a Crunch Automation
#'        keyword not yet supported by `crplyr`.
#' @name ca
#' @examples 
#' \dontrun{
#' ds %>%
#'     mutate(convert_to_text(ca$like("%other"), description = ca$copy))
#' }
#' @export
ca <- list(
  copy = noquote("COPY"),
  dots = function(x, y) noquote(paste0(alias(x), "...", alias(y))),
  like = function(x) noquote(paste0("LIKE(\"", x, "\")")),
  regex = function(x) noquote(paste0("REGEX(\"", x, "\")")),
  regexp = function(x) noquote(paste0("REGEXP(\"", x, "\")")),
  use_descriptions = noquote("USE DESCRIPTIONS"),
  use_titles = noquote("USE TITLES"),
  keyword = noquote
)
