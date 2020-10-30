crunch_auto_cmd <- function(formatter, get_aliases, ..., .data = NULL) {
  all_data <- c(list(...), .data)

  out <- list(list(formatter = formatter, get_aliases = get_aliases, data = all_data))
  class(out) <- c("crunch_auto_cmd", class(out))
  out
}

# Helper for creating commands out of the dots of other commands
# (and adds alias as the name of the object)
#' @importFrom purrr flatten
#' @importFrom utils modifyList
prepare_nested_cmds <- function(dots) {
  mod_dots <- lapply(seq_along(dots), function(dot_num) {
    dot <- dots[[dot_num]]
    if (is.data.frame(dot)) {
      as.list(dot)
    } else {
      setNames(list(dot), names(dots)[[dot_num]])
    }
  })
  mod_dots <- flatten(mod_dots)
  mod_dots_names <- names(mod_dots)

  mod_dots <- lapply(seq_along(mod_dots), function(dot_num) {
    dot <- mod_dots[[dot_num]]
    if (is_auto_cmd(dot)) {
      dot[[1]]$data <- modifyList(
        dot[[1]]$data,
        list(alias = mod_dots_names[dot_num])
      )
    }
    dot
  })
  names(mod_dots) <- mod_dots_names
  mod_dots
}

#' @importFrom purrr keep
#' @importFrom glue glue glue_collapse
nest_cmds <- function(cmd, dot_args) {
  intermediate_cmds <- keep(dot_args, is_auto_cmd)
  if (length(intermediate_cmds) == 0) return(cmd)

  nested_formatter <- function(x) {
    nested <- glue_collapse(
      lapply(x$.nested_data, function(cmd) format(cmd)),
      sep = "\n\n"
    )

    glue("{nested}\n\n{x$.main_formatter(x)}")
  }

  nested_get_aliases <- function(x) {
    nested <- lapply(x$.nested_data, function(nested_cmd) {
      aliases(nested_cmd)
    })
    main <- x$.main_get_aliases(x)

    unname(c(unlist(nested), unlist(main)))
  }

  crunch_auto_cmd(
    .data = cmd[[1]]$data,
    .main_formatter = cmd[[1]]$formatter,
    .main_get_aliases = cmd[[1]]$get_aliases,
    .nested_data = intermediate_cmds,
    formatter = nested_formatter,
    get_aliases = nested_get_aliases
  )
}

setMethod("format", "crunch_auto_cmd", function(x, ...) {
  x[[1]]$formatter(x[[1]]$data)
})

#' @export
#' @rdname crunch_var_df-meta
#' @importFrom crunch aliases
setMethod("aliases", "crunch_auto_cmd", function(x) {
  x[[1]]$get_aliases(x[[1]]$data)
})

#' @importFrom purrr flatten
#' @importFrom glue glue
#' @importFrom dplyr transmute
generate_commands <- function(var_df, ...) {
  .dots <- enquos(...)
  # Perform each dot separately because the result sometimes depends on the
  # name of the argument (for alias), which isn't available, and if you later use
  # that same name, `transmute` uses the result of the expression rather
  # than the original column in the dataset (and so that result would be
  # incomplete, because it hasn't received its name yet)
  all_commands <- lapply(seq_along(.dots), function(dot_num) {
    dot <- .dots[dot_num]
    dot_name <- names(.dots)[dot_num]

    dot_cmds <- transmute(var_df, !!!dot)

    if (is.data.frame(dot_cmds[[1]])) {
      warning(glue(
        "Found `data.frame` column named '{names(dot_cmds)[1]}', which usually happens ",
        "when an argument with `across()` is named. This name will be ignored, if you wanted ",
        "to change the names of the output columns use `across()` argument `.names=`."
      ), call. = FALSE)
      dot_cmds <- dot_cmds[[1]]
    }

    dot_cmds <- lapply(seq_along(dot_cmds), function(cmd_num) {
      cmd <- dot_cmds[[cmd_num]]
      cmd[[1]]$data <- modifyList(
        cmd[[1]]$data,
        list(alias = names(dot_cmds)[cmd_num])
      )
      cmd
    })

    var_df <<- add_placeholders_from_command(var_df, dot_cmds)

    dot_cmds
  })

  list(commands = flatten(all_commands), var_df = var_df)
}

#' @importFrom purrr map flatten_chr
add_placeholders_from_command <- function(var_df, cmds) {
  aliases <- map(cmds, aliases) # may include more than one
  aliases <- flatten_chr(aliases)

  var_placeholder_df <- structure(
    map(aliases, var_placeholder),
    .Names = aliases,
    row.names = c(NA, -1L),
    class = "data.frame"
  )

  var_df <- cbind(
    var_df[!names(var_df) %in% aliases],
    var_placeholder_df
  )
  structure(var_df, class = c("crunch_var_df", "data.frame"))
}

var_placeholder <- function(alias) {
  structure(
    list(alias = alias),
    class = c("var_placeholder", "list")
  )
}

#' @importFrom crunch is.variable
is_var_or_placeholder <- function(x) {
  is.variable(x) || inherits(x, "var_placeholder")
}

is_auto_cmd <- function(x) {
  inherits(x, "crunch_auto_cmd")
}

is_var_like <- function(x) {
  is_var_or_placeholder(x) || is_auto_cmd(x)
}

#' @importFrom glue glue glue_collapse
#' @importFrom purrr map_lgl
check_var_dots <- function(dots, command) {
  if (length(dots) == 0) stop(glue("No variables passed to function `{command}`"))

  types_ok <- map_lgl(dots, ~is.character(.) || is.numeric(.) || is_var_like(.))
  if (!all(types_ok)) {
    bad_pos <- which(!types_ok)
    bad_names <- names(dots[bad_pos])
    if (is.null(bad_names)) {
      bad_descriptor <- bad_pos
    } else {
      bad_descriptor <- ifelse(bad_names != "", bad_names, bad_pos)
    }

    stop(glue(
      "Expected all arguments to ... of `{command}` to be Crunch variables, ca keywords",
      " (from `ca$` functions), strings, or numbers. Arguments ",
      "{glue_collapse(bad_descriptor, sep = ", ")} were not."
    ))
  }
}

check_var_single <- function(x, command) {
  if (!(is.character(x) || is.numeric(x) || is_var_like(x))) {
    stop(glue(
      "Expected x in `{command}` to be Crunch variables, ca keywords",
      " (from `ca$` functions), strings, or numbers. Arguments ",
      "{glue_collapse(bad_descriptor, sep = ", ")} were not."
    ))
  }
}

#' Use Argument Values Based on Input Variables
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

#' @importFrom rlang is_function is_formula as_function
ca_process_formula <- function(x, data) {
  if (is_function(x) || is_formula(x)) {
    f <- as_function(x)
    x <- f(data)
  }
  x
}

check_for_int_in_character <- function(x) {
  if (is.character(x) && any(grepl("^-?[:digit:]+$", x))) {
    warning(paste0(
      "Character vector contains things that look like numbers. ",
      "Did you use `c()` instead of `list()`?"
    ))
  }
}


#' @importFrom rlang is_formula f_lhs f_rhs f_env eval_bare
#' @importFrom dplyr tibble
ca_cat_formulas_to_df <- function(formula) {
  if (!is_formula(formula)) {
    stop("Expected a formula describing the category.")
  }

  lhs <- eval_bare(f_lhs(formula), f_env(formula))
  lhs <- if (!is.list(lhs) || length(lhs) != 1) list(lhs) else lhs
  lhs <- tibble(from = lhs)

  rhs <- eval_bare(f_rhs(formula), f_env(formula))

  bind_cols(lhs, rhs)
}

# Modified formula to df function that handles the expressions
# in lhs of case when & non-categories in RHS unique to
# case when
#' @importFrom rlang is_formula f_lhs f_rhs f_env eval_bare
#' @importFrom dplyr tibble
ca_cat_case_formulas_to_df <- function(formula)  {
  if (!is_formula(formula)) {
    stop("Expected a formula describing the case.")
  }

  # TODO: To save time on initial implementation this just captures
  # the text as the user wrote them in the function, but it should
  # use true formulas (which would require handling var placeholders
  # & parentheses)
  lhs <-  deparse1(f_lhs(formula))
  lhs <- tibble(expr = lhs)

  rhs <- eval_bare(f_rhs(formula), f_env(formula))

  if (is_var_like(rhs)) rhs <- tibble(label = NA, variable = alias(rhs))
  if (is.null(rhs)) rhs <- tibble(label = NA)

  bind_cols(lhs, rhs)
}

is_ca_label_df <- function(x) {
  is.data.frame(x) && nrow(x) == 1 && "label" %in% names(x)
}
