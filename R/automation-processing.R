#' @importFrom purrr flatten
run_steps <- function(var_df, ...) {
  .dots <- enquos(...)
  # Perform each dot separately because the result sometimes depends on the
  # name of the argument (for alias), which isn't available, and if you later use
  # that same name, `transmute` uses the result of the expression rather
  # than the original column in the dataset (and so that result would be 
  # incomplete, because it hasn't received its name yet)
  all_steps <- lapply(seq_along(.dots), function(dot_num) {
    dot <- .dots[dot_num]
    dot_name <- names(.dots)[dot_num]
    
    dot_steps <- transmute(var_df, !!!dot)
    
    if (is.data.frame(dot_steps[[1]])) {
      warning(glue::glue(
        "Found `data.frame` column named '{names(dot_steps)[1]}', which usually happens ",
        "when an argument with `across()` is named. This name will be ignored, if you wanted ",
        "to change the names of the output columns use `across()` argument `.names=`."
      ), call. = FALSE)
      dot_steps <- dot_steps[[1]]
    }

    dot_steps <- lapply(seq_along(dot_steps), function(step_num) {
      step <- dot_steps[[step_num]]
      step[[1]]$data <- modifyList(
        step[[1]]$data,
        list(alias = names(dot_steps)[step_num])
      )
      step
    })
    
    var_df <<- add_placeholders_from_command(var_df, dot_steps)
    
    dot_steps
  })
  
  list(steps = flatten(all_steps), var_df = var_df)
}

#' @importFrom purrr flatten_chr
add_placeholders_from_command <- function(var_df, steps) {
  aliases <- lapply(steps, aliases)
  aliases <- flatten_chr(aliases)
  
  var_placeholder_df <- structure(
    lapply(aliases, var_placeholder),
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

is_var_or_placeholder <- function(x) {
  is.variable(x) || inherits(x, "var_placeholder")
}

is_auto_cmd <- function(x) {
  inherits(x, "crunch_auto_cmd")
}

is_var_like <- function(x) {
  is_var_or_placeholder(x) || is_auto_cmd(x)
}

crunch_auto_cmd <- function(formatter, get_aliases, ..., .data = NULL) {
  all_data <- c(list(...), .data)
  
  out <- list(list(formatter = formatter, get_aliases = get_aliases, data = all_data))
  class(out) <- c("crunch_auto_cmd", class(out))
  out
}

prepare_nested_cmds <- function(dots) {
  mod_dots <- lapply(seq_along(dots), function(dot_num) {
    dot <- dots[[dot_num]]
    if (is.data.frame(dot)) {
      as.list(dot)
    } else {
      setNames(list(dot), names(dots)[[dot_num]])
    }
  })
  mod_dots <- purrr::flatten(mod_dots)
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

nest_cmds <- function(cmd, dot_args) {
  intermediate_cmds <- purrr::keep(dot_args, is_auto_cmd)
  if (length(intermediate_cmds) == 0) return(cmd)
  
  nested_formatter <- function(x) {
    nested <- glue::glue_collapse(
      lapply(x$.nested_data, function(step) format(step)),
      sep = "\n\n"
    )
    
    glue::glue("{nested}\n\n{x$.main_formatter(x)}")
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

setMethod("aliases", "crunch_auto_cmd", function(x) {
  x[[1]]$get_aliases(x[[1]]$data)
})

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

ca_process_formula <- function(x, data) {
  if (rlang::is_function(x) || rlang::is_formula(x)) {
    f <- rlang::as_function(x)
    x <- f(data)
  } 
  x
}
