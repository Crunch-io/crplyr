# Create arrays ----
#' Gather Variables into a Categorical Array
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
#' @param hide_inputs A logical indicating whether to move the variables from collected
#' into the new array variable into the hidden folder
#' @param title Optional, the title of the variables created (also called the variable name
#' in the crunch R package)
#' @param description Optional, the description of the variables created
#' @param notes Optional, the notes of the variable created
#' @family creating variables functions
#' @export
categorical_array <- function(
  ...,
  labels = NULL,
  hide_inputs = FALSE,
  title = NULL,
  description = NULL,
  notes = NULL
) {
  .dots <- list(...)
  check_var_dots(.dots, "categorical_array()")
  .dots <- prepare_nested_cmds(.dots)

  .vars <- crunch_var_df_from_dots(.dots)
  sv_aliases <- internal_aliases(.vars)

  labels <- ca_process_formula(labels, .vars)
  title <- ca_process_formula(title, .vars)
  description <- ca_process_formula(description, .vars)
  notes <- ca_process_formula(notes, .vars)

  stopifnot(is.logical(hide_inputs))

  cmd <- crunch_auto_cmd(
    formatter = ca_template(
      "CREATE CATEGORICAL ARRAY",
      "{ca_list_to_text(items = sv_aliases, indent = 2)}",
      "{ca_list_to_text('LABELS', indent = 2)}",
      "{ca_text_if(hide_inputs, '\n  HIDE INPUTS')}",
      "\nAS {alias}",
      "{ca_list_to_text('TITLE', title)}",
      "{ca_list_to_text('DESCRIPTION', description)}",
      "{ca_list_to_text('NOTES', notes)}",
      ";"
    ),
    get_aliases = function(x) x$alias,
    sv_aliases = sv_aliases,
    labels = labels,
    hide_inputs = hide_inputs,
    title = title,
    description = description,
    notes = notes
  )

  nest_cmds(cmd, .dots)
}

#' Gather Variables into a Multiple Dichotomy
#'
#' Create a Multiple Dichotomy from one or more existing variables.
#' The new variable's alias is based on the name of the argument within
#' `mutate()`. Based on the Crunch Automation command
#' [CREATE MULTIPLE DICHOTOMY](https://help.crunch.io/hc/en-us/articles/360042039552).
#'
#' If `...` is specified using aliases or `dplyr::across()`, then
#' the other arguments can take functions based on the variables
#' selected using [`crplyr-formula-notation`].
#'
#' @inheritParams categorical_array
#' @param selections A vector of either strings or numbers indicating the category names
#' or codes that should be treated as the selected category. Note that R's `c()` function
#' will convert numbers to text if both are included, so if you want to specify both, use
#' the `list()` command instead.
#' @family creating variables functions
#' @export
multiple_dichotomy <- function(
  ...,
  labels = NULL,
  selections = NULL,
  hide_inputs = FALSE,
  title = NULL,
  description = NULL,
  notes = NULL
) {
  .dots <- list(...)
  check_var_dots(.dots, "multiple_dichotomy()")
  .dots <- prepare_nested_cmds(.dots)

  .vars <- crunch_var_df_from_dots(.dots)
  sv_aliases <- internal_aliases(.vars)

  labels <- ca_process_formula(labels, .vars)
  title <- ca_process_formula(title, .vars)
  description <- ca_process_formula(description, .vars)
  notes <- ca_process_formula(notes, .vars)
  selections <- ca_process_formula(selections, .vars)

  stopifnot(is.logical(hide_inputs))
  if (!all(map_lgl(selections, ~is.character(.) | is.numeric(.)))) {
    stop("Expected all selections to be category names (character) or codes (numeric)")
  }
  check_for_int_in_character(selections)

  cmd <- crunch_auto_cmd(
    formatter = ca_template(
      "CREATE MULTIPLE DICHOTOMY",
      "{ca_list_to_text(items = sv_aliases, indent = 2)}",
      "{ca_list_to_text('LABELS', labels, indent = 2)}",
      "{ca_list_to_text('SELECTED', selections, indent = 2)}",
      "{ca_text_if(hide_inputs, '\n  HIDE INPUTS')}",
      "\nAS {alias}",
      "{ca_list_to_text('TITLE', title)}",
      "{ca_list_to_text('DESCRIPTION', description)}",
      "{ca_list_to_text('NOTES', notes)}",
      ";"
    ),
    get_aliases = function(x) x$alias,
    sv_aliases = sv_aliases,
    labels = labels,
    hide_inputs = hide_inputs,
    title = title,
    description = description,
    notes = notes,
    selections = selections
  )

  nest_cmds(cmd, .dots)
}

#' Gather Variables into a Multiple Selection
#'
#' Create a Multiple Selection from one or more existing variables.
#' The new variable's alias is based on the name of the argument within
#' `mutate()`. Based on the Crunch Automation command
#' [CREATE MULTIPLE SELECTION](https://help.crunch.io/hc/en-us/articles/360044079971).
#'
#' If `...` is specified using aliases or `dplyr::across()`, then
#' the other arguments can take functions based on the variables
#' selected using [`crplyr-formula-notation`].
#'
#' @inheritParams categorical_array
#' @param not_selected A vector of either strings or numbers indicating the category names
#' or codes that should be ignored. Note that R's `c()` function
#' will convert numbers to text if both are included, so if you want to specify both, use
#' the `list()` command instead.
#' @param exclude_empty Whether to exclude empty data.
#' @family creating variables functions
#' @export
multiple_selection <- function(
  ...,
  labels = NULL,
  not_selected = NULL,
  exclude_empty = FALSE,
  hide_inputs = FALSE,
  title = NULL,
  description = NULL,
  notes = NULL
) {
  .dots <- list(...)
  check_var_dots(.dots, "multiple_selections()")
  .dots <- prepare_nested_cmds(.dots)

  .vars <- crunch_var_df_from_dots(.dots)
  sv_aliases <- internal_aliases(.vars)

  labels <- ca_process_formula(labels, .vars)
  title <- ca_process_formula(title, .vars)
  description <- ca_process_formula(description, .vars)
  notes <- ca_process_formula(notes, .vars)
  selections <- ca_process_formula(selections, .vars)

  stopifnot(is.logical(hide_inputs))
  if (!all(map_lgl(not_selected, ~is.character(.) | is.numeric(.)))) {
    stop("Expected all not_selected to be category names (character) or codes (numeric)")
  }
  check_for_int_in_character(not_selected)

  cmd <- crunch_auto_cmd(
    formatter = ca_template(
      "CREATE MULTIPLE DICHOTOMY",
      "{ca_list_to_text(items = sv_aliases, indent = 2)}",
      "{ca_list_to_text('LABELS', labels, indent = 2)}",
      "{ca_list_to_text('NOT SELECTED', not_selected, indent = 2)}",
      "{ca_text_if(hide_inputs, '\n  HIDE INPUTS')}",
      "\nAS {alias}",
      "{ca_list_to_text('TITLE', title)}",
      "{ca_list_to_text('DESCRIPTION', description)}",
      "{ca_list_to_text('NOTES', notes)}",
      ";"
    ),
    get_aliases = function(x) x$alias,
    sv_aliases = sv_aliases,
    labels = labels,
    hide_inputs = hide_inputs,
    title = title,
    description = description,
    notes = notes,
    selections = selections
  )

  nest_cmds(cmd, .dots)
}

# Create Categorical Variables ----
#' Create a Categorical Variable by Assign Expressions to Categories & Variables
#'
#' Create a Categorical Variable by assigning expressions variables to either
#' categories directly or to the values of existing categorical variables.
#' The new variable's alias is based on the name of the argument within
#' `mutate()`. Based on the Crunch Automation command
#' [CREATE CATEGORICAL CASE](https://help.crunch.io/hc/en-us/articles/360042039192) &
#' [CREATE CATEGORICAL CASE THEN VARIABLE](https://help.crunch.io/hc/en-us/articles/360042457871).
#'
#' @inheritParams categorical_array
#' @param ..., cases Expressions where the right hand side is a Crunch logical
#' expression (or `TRUE` to indicate that it should be met if all other
#' expressions were not met) and the right hand side is a category or
#' existing categorical variable.
#'
#' @family creating variables functions
#' @importFrom purrr keep discard pmap map_dfr map_lgl
#' @importFrom rlang is_formula
#' @export
categorical_case_when <- function(
  ...,
  cases = NULL,
  title = NULL,
  description = NULL,
  notes = NULL
) {
  .dots <- list(...)
  formula_cases <- c(keep(.dots, is_formula), keep(cases, is_formula))
  non_formula_cases <- c(discard(.dots, is_formula), discard(.dots, is_formula))

  if (any(!map_lgl(non_formula_cases, is.data.frame))) {
    stop("All cases must be formulas or data.frames.")
  }

  formula_dfs <- map_dfr(formula_cases, ca_cat_case_formulas_to_df)
  cases_df <- bind_rows(non_formula_cases, formula_dfs)
  expr_list <- cat_cases_df_to_text(cases_df)

  cmd <- crunch_auto_cmd(
    formatter = ca_template(
      "CREATE CATEGORICAL",
      "{ca_list_to_text('CASE', expr_list, indent = 2, sep_newline = TRUE)}",
      "\nAS {alias}",
      "{ca_list_to_text('TITLE', title)}",
      "{ca_list_to_text('DESCRIPTION', description)}",
      "{ca_list_to_text('NOTES', notes)}",
      ";"
    ),
    get_aliases = function(x) x$alias,
    expr_list = expr_list,
    title = title,
    description = description,
    notes = notes
  )
}

#' Create a Categorical Variable by Cutting a Numeric Variable into Categories
#'
#' Create a Categorical Variable by cutting the numeric values into categories
#' where values are between the breakpoints provided by `breaks`. The new variable's
#' alias is based on the aliases in `new_aliases` by default, but if it is
#' `NULL` and there is only a single variable, then it is based on
#' the name of the argument within `mutate()`.
#'  Based on the Crunch Automation command
#' [CREATE CATEGORICAL CUT](https://help.crunch.io/hc/en-us/articles/360042458431)
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
#' [`crplyr-formula-notation`] that creates one).
#' @param breaks TKTKTK
#' @param closed TKTKTK
#' @param labels TKTKTK
#' @param missing TKTKTK
#'
#' @family creating variables functions
#' @importFrom purrr map modify_if
#' @export
categorical_cut <- function(
  ...,
  breaks = NULL,
  closed = NULL,
  labels = NULL,
  missing = NULL,
  new_aliases = NULL,
  title = NULL,
  description = NULL,
  notes = NULL
) {
  .dots <- list(...)
  check_var_dots(.dots, "categorical_cut()")
  .dots <- prepare_nested_cmds(.dots)

  .vars <- crunch_var_df_from_dots(.dots)
  old_aliases <- internal_aliases(.vars)

  new_aliases <- ca_process_formula(new_aliases, .vars)
  title <- ca_process_formula(title, .vars)
  description <- ca_process_formula(description, .vars)
  notes <- ca_process_formula(notes, .vars)

  # labels get breaks rather than variable names
  labels <- ca_process_formula(labels, breaks)
  labels <- modify_if(labels, is.character, ca$category)
  labels <- map(labels, ca_cat_df_to_text)

  missing <- modify_if(missing, is.numeric, ~tibble(from = ., label = NA))
  missing <- modify_if(missing, is_formula, ca_cat_formulas_to_df)
  if (!all(map_lgl(missing, is.data.frame))) {
    stop("Expected the missing categories to be formulas, data.frames or numeric values.")
  }
  missing <- pmap(bind_rows(missing), ca_cat_to_text, use_from = TRUE)

  if (is.null(closed)) {
    break_closed_text <- ""
  } else {
    break_closed_text <- paste0("CLOSED ", closed)
  }

  cmd <- crunch_auto_cmd(
    formatter = function(x) {
      if (is.null(x$new_aliases)) {
        x$new_aliases <- list(noquote(x$alias))
      } else {
        x$new_aliases <- map(x$new_aliases, noquote)
      }

      template <- ca_template(
        "CREATE CATEGORICAL CUT",
        "{ca_list_to_text(items = old_aliases, indent = 2)}",
        "{ca_list_to_text('BREAKS', breaks, break_closed_text, indent = 2)}",
        "{ca_list_to_text('LABELS', labels, indent = 2)}",
        "{ca_list_to_text('SET MISSING', missing, indent = 2)}",
        "{ca_list_to_text('AS', new_aliases)}",
        "{ca_list_to_text('TITLE', title)}",
        "{ca_list_to_text('DESCRIPTION', description)}",
        "{ca_list_to_text('NOTES', notes)}",
        ";"
      )
      template(x)
    },
    get_aliases = function(x) if (is.null(x$new_aliases)) x$alias else x$new_aliases,
    old_aliases = old_aliases,
    breaks = breaks,
    break_closed_text = break_closed_text,
    labels = labels,
    missing = missing,
    new_aliases = new_aliases,
    title = title,
    description = description,
    notes = notes
  )

  nest_cmds(cmd, .dots)
}

#' Create a Categorical Variable by Interacting Categories of Two or More Variables
#'
#' Create a Categorical Variable by interacting multiple existing categorical variables
#' and each combination of categories becomes a category in the final variable. The new variable's
#' alias is the name of the argument within `mutate()`.
#'  Based on the Crunch Automation command
#' [CREATE CATEGORICAL INTERACTION](https://help.crunch.io/hc/en-us/articles/360043695471)
#'
#' If `...` is specified using aliases or `dplyr::across()`, then
#' the other arguments can take functions based on the variables
#' selected using [`crplyr-formula-notation`].
#'
#' @inheritParams categorical_cut
#' @param labels TKTKTK (slightly different than cut, so needs doc)
#' @family creating variables functions
#' @importFrom purrr map
#' @export
categorical_interaction <- function(
  ...,
  labels = NULL,
  title = NULL,
  description = NULL,
  notes = NULL
) {
  .dots <- list(...)
  check_var_dots(.dots, "categorical_interaction()")
  .dots <- prepare_nested_cmds(.dots)

  .vars <- crunch_var_df_from_dots(.dots)
  old_aliases <- internal_aliases(.vars)

  title <- ca_process_formula(title, .vars)
  description <- ca_process_formula(description, .vars)
  notes <- ca_process_formula(notes, .vars)

  if (!is.data.frame(labels)) labels <- map_dfr(labels, ca_cat_formulas_to_df)
  labels$from <- map(
    labels$from,
    ~noquote(glue("({ca_list_to_text(items = ., start_newline = FALSE)})"))
  )
  labels <- ca_cat_df_to_text(labels, use_from = TRUE)

  cmd <- crunch_auto_cmd(
    formatter = ca_template(
      "CREATE CATEGORICAL INTERACTION",
      "{ca_list_to_text(items = old_aliases, indent = 2)}",
      "{ca_list_to_text('WITH', labels, indent = 2, sep_newline = TRUE)}",
      "{ca_list_to_text('AS', alias)}",
      "{ca_list_to_text('TITLE', title)}",
      "{ca_list_to_text('DESCRIPTION', description)}",
      "{ca_list_to_text('NOTES', notes)}"
      ),
    get_aliases = function(x) x$alias,
    old_aliases = old_aliases,
    labels = labels,
    title = title,
    description = description,
    notes = notes
  )

  nest_cmds(cmd, .dots)
}

#' Create Categorical Variables by Recoding categories of Existing Variables
#'
#' Create a Categorical Variable by interacting multiple existing categorical variables
#' and each combination of categories becomes a category in the final variable. The new variable's
#' alias is the name of the argument within `mutate()`.
#'  Based on the Crunch Automation command
#' [CREATE CATEGORICAL RECODE](https://help.crunch.io/hc/en-us/articles/360043695471)
#'
#' If `...` is specified using aliases or `dplyr::across()`, then
#' the other arguments can take functions based on the variables
#' selected using [`crplyr-formula-notation`].
#'
#' @inheritParams categorical_cut
#' @param labels TKTKTK (slightly different than cut, so needs doc)
#' @family creating variables functions
#' @importFrom purrr map
#' @export
categorical_interaction <- function(
    ...,
    labels = NULL,
    title = NULL,
    description = NULL,
    notes = NULL
) {
    .dots <- list(...)
    check_var_dots(.dots, "categorical_interaction()")
    .dots <- prepare_nested_cmds(.dots)

    .vars <- crunch_var_df_from_dots(.dots)
    old_aliases <- internal_aliases(.vars)

    title <- ca_process_formula(title, .vars)
    description <- ca_process_formula(description, .vars)
    notes <- ca_process_formula(notes, .vars)

    if (!is.data.frame(labels)) labels <- map_dfr(labels, ca_cat_formulas_to_df)
    labels$from <- map(
        labels$from,
        ~noquote(glue("({ca_list_to_text(items = ., start_newline = FALSE)})"))
    )
    labels <- ca_cat_df_to_text(labels, use_from = TRUE)

    cmd <- crunch_auto_cmd(
        formatter = ca_template(
            "CREATE CATEGORICAL INTERACTION",
            "{ca_list_to_text(items = old_aliases, indent = 2)}",
            "{ca_list_to_text('WITH', labels, indent = 2, sep_newline = TRUE)}",
            "{ca_list_to_text('AS', alias)}",
            "{ca_list_to_text('TITLE', title)}",
            "{ca_list_to_text('DESCRIPTION', description)}",
            "{ca_list_to_text('NOTES', notes)}"
        ),
        get_aliases = function(x) x$alias,
        old_aliases = old_aliases,
        labels = labels,
        title = title,
        description = description,
        notes = notes
    )

    nest_cmds(cmd, .dots)
}

# Convert ----
#' Convert a Variable's Type
#'
#' When called from [`mutate`], converts the variables type either as a new variable, or in place
#' (in which case it is considered a "schema command" and must be run before any
#' "non-schema commands").
#' Based on the Crunch Automation commands
#' [CONVERT](https://help.crunch.io/hc/en-us/articles/360042247191) and
#' [CREATE CONVERT](https://help.crunch.io/hc/en-us/articles/360047136371).
#'
#' If `...` is specified using aliases or `dplyr::across()`, then
#' the other arguments can take functions based on the variables
#' selected using [`crplyr-formula-notation`].
#'
#' @inheritParams categorical_array
#' @inheritParams categorical_cut
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
  convert_base(
    "text",
    ...,
    new_aliases = new_aliases,
    title = title,
    description = description,
    notes = notes
  )
}

#' @export
#' @rdname convert_to_text
convert_to_numeric <- function(
  ...,
  new_aliases = NULL,
  title = NULL,
  description = NULL,
  notes = NULL
) {
  convert_base(
    "numeric",
    ...,
    new_aliases = new_aliases,
    title = title,
    description = description,
    notes = notes
  )
}

#' @export
#' @rdname convert_to_text
convert_to_datetime <- function(
  ...,
  format = NULL,
  resolution = NULL,
  new_aliases = NULL,
  title = NULL,
  description = NULL,
  notes = NULL
) {
  if (is.null(format)) stop("Must specify a format for `convert_to_datetime()`")

  convert_base(
    "datetime",
    ...,
    new_aliases = new_aliases,
    title = title,
    description = description,
    notes = notes,
    format = format,
    resolution = resolution
  )
}

#' @export
#' @rdname convert_to_text
#' @importFrom purrr keep discard map_lgl map_dfr
#' @importFrom rlang is_formula
convert_to_categorical <- function(
  ...,
  categories = NULL,
  new_aliases = NULL,
  title = NULL,
  description = NULL,
  notes = NULL
) {
  .dots <- list(...)

  formula_dots <- keep(.dots, is_formula)
  dots_category_df <- map_dfr(formula_dots, ca_cat_formulas_to_df)
  if (is.list(categories) && all(map_lgl(categories, is_formula))) {
    categories <- map_dfr(categories, ca_cat_formulas_to_df)
  }
  if (length(categories) > 0 && !is.data.frame(categories)) {
    stop("Expected categories to be a list of formulas or a data.frame")
  }

  categories <- bind_rows(categories, dots_category_df)
  categories <- ca_cat_df_to_text(categories, use_from = TRUE, after_from = " TO ")
  categories <- map(categories, ~noquote(paste0("VALUE ", .)))

  .dots <- discard(.dots, is_formula)

  convert_base(
    "categorical",
    .dots = .dots,
    new_aliases = new_aliases,
    title = title,
    description = description,
    notes = notes,
    categories = categories
  )
}

convert_base <- function(
  type,
  ...,
  new_aliases,
  title,
  description,
  notes,
  format = NULL,
  resolution = NULL,
  categories = NULL,
  .dots = NULL
) {
  .dots <- c(.dots, list(...))
  check_var_dots(.dots, paste0("convert_to_", type, "()"))
  .dots <- prepare_nested_cmds(.dots)

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
          "{ca_list_to_text('CONVERT', old_aliases, type, start_newline = FALSE)}",
          "{ca_list_to_text(' FORMAT', format, start_newline = FALSE)}",
          "{ca_list_to_text(' RESOLUTION', resolution, start_newline = FALSE)}",
          "{ca_list_to_text(' WITH', categories, sep_newline = TRUE, start_newline = FALSE)}",
          ";"
        )
      } else {
        template <- ca_template(
          "{ca_list_to_text('CREATE CONVERT', old_aliases, type, start_newline = FALSE)}",
          "{ca_list_to_text(' FORMAT', format, start_newline = FALSE)}",
          "{ca_list_to_text(' RESOLUTION', resolution, start_newline = FALSE)}",
          "{ca_list_to_text(' WITH', categories, sep_newline = TRUE, start_newline = FALSE)}",
          "{ca_list_to_text('AS', new_aliases)}",
          "{ca_list_to_text('TITLE', title)}",
          "{ca_list_to_text('DESCRIPTION', description)}",
          "{ca_list_to_text('NOTES', notes)}",
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
    vars_have_ca_expansion = vars_have_ca_expansion,
    type = glue("TO {toupper(type)}"),
    format = format,
    resolution = resolution,
    categories = categories
  )

  nest_cmds(cmd, .dots)
}

# Alter Variables ----
#' Add or Remove Subvariables to an Array Variable
#'
#' Modify an existing array variable by adding or removing subvariables.
#' Removing subvariables is a schema command and so must be run before any
#' non-schema commands are run.
#' Based on the Crunch Automation command
#' [ALTER ARRAY](https://help.crunch.io/hc/en-us/articles/360045427871)
#'
#'
#' @param x TKTKTK
#' @param ... TKTKTK
#' @param position TKTKTK
#' @family creating variables functions
#' @export
add_to_array <- function(x, ..., position = Inf) {
  check_var_single(x, "alter_array()")
  x <- prepare_nested_cmds(list(x))
  .x_var <- crunch_var_df_from_dots(x)
  x_alias <- internal_aliases(.x_var)

  .dots <- list(...)
  check_var_dots(.dots, "add_to_array()")
  .dots <- prepare_nested_cmds(.dots)

  .add_vars <- crunch_var_df_from_dots(.dots)
  add_aliases <- internal_aliases(.add_vars)

  if (is.infinite(position)) position <- ca$keyword("POSITION LAST")
  else if (position == 0) position <- ca$keyword("POSITION FIRST")
  else if (is.numeric(position)) position <- ca$keyword(paste0("POSITION "), position)
  # TODO: aliases in before/after refer to subvariable aliases so tidy eval
  # might need to be modified
  position <- ca_quote_items(position)

  cmd <- crunch_auto_cmd(
    formatter = ca_template(
      "ALTER ARRAY {x_alias} ",
      "{ca_list_to_text('ADD', add_aliases, position, start_newline = FALSE)}"
    ),
    get_aliases = function(x) x$alias,
    x_alias = x_alias,
    add_aliases = add_aliases,
    position = position
  )

  out <- nest_cmds(cmd, list(x))
  out <- nest_cmds(out, .dots)
  out
}

#' @rdname add_to_array
#' @export
remove_from_array <- function(x, ...) {
  check_var_single(x, "alter_array()")
  x <- prepare_nested_cmds(list(x))
  .x_var <- crunch_var_df_from_dots(x)
  x_alias <- internal_aliases(.x_var)

  .dots <- enquos(...)
  browser()
  check_var_dots(.dots, "remove_from_array()")
  .dots <- prepare_nested_cmds(.dots)

  .rm_vars <- crunch_var_df_from_dots(.dots)
  rm_aliases <- internal_aliases(.rm_vars)


  cmd <- crunch_auto_cmd(
    formatter = ca_template(
      "ALTER ARRAY {x_alias} ",
      "{ca_list_to_text('REMOVE', rm_aliases, start_newline = FALSE)}"
    ),
    get_aliases = function(x) x$alias,
    x_alias = x_alias,
    rm_aliases = rm_aliases
  )

  out <- nest_cmds(cmd, list(x))
  out <- nest_cmds(out, .dots)
  out
}
