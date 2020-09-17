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
      "{ca_list_to_text('NOTES',  notes)}", 
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
      "{ca_list_to_text('NOTES',  notes)}", 
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
#' @param new_aliases If only one variable is specified, this argument is ignored,
#' the alias comes from the name of the argument in mutate (and if it's the same
#' the variable will be modified in place, while if different it will create a new
#' variable). If more than one variable is specified `NULL`, the default, will modify
#' the variables in place, or a vector of aliases for the new variables (or a 
#' [`crplyr-formula-notation`] that creates one). 
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
#' @importFrom purrr keep discard map_lgl
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
  dots_category_df <- cat_convert_formulas_to_df(formula_dots)
  if (is.list(categories) && all(map_lgl(categories, is_formula))) {
    categories <- cat_convert_formulas_to_df(formula_dots)
  }
  if (!(is.null(categories) | is.data.frame(categories))) {
    stop("Expected categories to be a list of formulas or a data.frame")
  }
  
  categories <- c(
    cat_convert_values_df_to_text(dots_category_df),
    cat_convert_values_df_to_text(categories)
  )
  
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

#' @importFrom rlang f_lhs f_rhs f_env eval_bare
#' @importFrom purrr map_dfr
cat_convert_formulas_to_df <- function(formulas) {
  map_dfr(formulas, function(fm) {
    from <- eval_bare(f_lhs(fm), f_env(fm))
    label_info <- eval_bare(f_rhs(fm), f_env(fm))
    
    if (!(is.character(from) | is.numeric(from) | length(from) != 1)) {
      stop ("Expected a single string or number in right hand side of formula.")
    }
    
    if (is.character(label_info)) label_info <- ca$category(label_info)
    if (!is_ca_label_df(label_info)) {
      stop(
        "Expected a label object (like one created by `ca$category()`) in left hand side of formula."
      )
    }
    
    label_info$from <- list(from)
    label_info
  })
}

#' @importFrom purrr pmap_chr
#' @importFrom glue glue
cat_convert_values_df_to_text <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (
    !is.data.frame(df) || 
    !all(c("from", "label") %in% names(df)) || 
    length(setdiff(names(df), c("from", "label", "code", "missing"))) > 0
  ) {
    stop("Expected a data.frame with columns 'from' and 'label' (and optionally 'code' and 'missing')")
  }
  
  if (!"code" %in% names(df)) df$code <- NA
  if (!"missing" %in% names(df)) df$missing <- FALSE
  
  pmap(df, function(from, label, code, missing) {
    from <- ca_quote_items(from)
    label <- ca_quote_items(label)
    if (!is.na(code)) code <- glue(" CODE {code}") else code <- ""
    if (!is.na(missing) & missing) missing <- " MISSING" else missing <- ""
    
    noquote(glue("VALUE {from} TO {label}{code}{missing}"))
  })
}

# Alter Variables ----
