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
    
    var_df <<- add_steps_to_var_df(var_df, dot_steps)
    
    dot_steps
  })
  
  list(steps = flatten(all_steps), var_df = var_df)
}

internalize_arg_names <- function(x, name_as) {
  lapply(names(x), function(x_name) {
    out <- x[[x_name]]
    out[[1]]$data <- modifyList(out[[1]]$data, setNames(list(x_name), name_as))
    out
  })
}

#' @importFrom purrr flatten_chr
add_steps_to_var_df <- function(var_df, steps) {
  aliases <- lapply(steps, function(step) step[[1]]$get_aliases(step[[1]]$data))
  aliases <- flatten_chr(aliases)
  var_df <- cbind(
    var_df[!names(var_df) %in% aliases], 
    var_placeholder_df(aliases)
  )
  structure(var_df, class = c("crunch_var_df", "data.frame"))
}

var_placeholder_df <- function(aliases) {
  structure(
    setNames(lapply(aliases, function(alias) structure(list(alias = alias), class = c("var_placeholder", "list"))), aliases), 
    row.names = c(NA, -1L),
    class = "data.frame"
    )
}

is_var_or_similar <- function(x) {
  is.variable(x) || inherits(x, "var_placeholder") || inherits(x, "crunch_auto_cmd")
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

crunch_auto_cmd <- function(formatter, get_aliases, ...) {
  out <- list(list(formatter = formatter, get_aliases = get_aliases, data = list(...)))
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
    if (inherits(dot, "crunch_auto_cmd")) {
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
  intermediate_cmds <- purrr::keep(dot_args, ~inherits(., "crunch_auto_cmd"))
  if (length(intermediate_cmds) == 0) return(cmd)
  
  new_data <- c(
    cmd[[1]]$data,
    list(
      .main_formatter = cmd[[1]]$formatter,
      .main_get_aliases = cmd[[1]]$get_aliases,
      .nested_data = intermediate_cmds,
      formatter = function(x) {
        nested <- glue::glue_collapse(
          lapply(x$.nested_data, format_ca_auto),
          sep = "\n\n"
        )
        orig_data <- x[setdiff(names(x), c(".main_formatter", ".main_get_aliases", ".nested_data"))]
        
        glue::glue("{nested}\n\n{x$.main_formatter(orig_data)}")
      },
      get_aliases = function(x) {
        nested <- lapply(x$.nested_data, function(nested_cmd) {
          nested_cmd[[1]]$get_aliases(nested_cmd[[1]]$data)
        })
        orig_data <- x[setdiff(names(x), c(".main_formatter", ".main_get_aliases", ".nested_data"))]
        main <- x$.main_get_aliases(orig_data)
        
        unname(c(unlist(nested), unlist(main)))
      }
    )
  )
  do.call(crunch_auto_cmd, new_data)
}

format_ca_auto <- function(x, ...) {
  x[[1]]$formatter(x[[1]]$data)
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
