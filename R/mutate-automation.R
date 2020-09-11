#' @export
as_crunch_auto_tbl <- function(x) { # TODO: move to a S4 initializer?
  out <- AutoReadyCrunchDataset(x)
  out@steps <- list()
  out@var_tibble <- crunch_var_df(x)
  out
}

# A data.frame with list columns containing crunch variables
# `tibble` doesn't allow this data.frame because CrunchVars
# are not true "vectors" so be careful of manipulating this
crunch_var_df <- function(dataset) {
  all_aliases <- aliases(allVariables(dataset))
  var_list <- lapply(all_aliases, function(alias) dataset[[alias]])
  names(var_list) <- all_aliases
  structure(var_list, row.names = c(NA, -1L), class = "data.frame")
}

calculate_steps <- function(var_df, ...) {
  steps <- transmute(var_df, ...)
  
  lapply(names(steps), function(step_alias) {
    out <- steps[[step_alias]][[1]]
    out$data <- modifyList(out$data, list(alias = step_alias))
    out
  })
}


#' @export
mutate.CrunchDataset <- function(.data, ...) {
  if (!inherits(.data, "AutoReadyCrunchDataset")) .data <- as_crunch_auto_tbl(.data)
  
  out <- .data
  out@steps <- c(out@steps, calculate_steps(out@var_tibble, ...))
  out
}

make_query_text <- function(x) {
  steps <- lapply(x@steps, format_ca_auto)
  if (length(steps) == 0) return(NULL)  
  paste(steps, collapse = "\n\n")
}

#' @export
show_query.AutoReadyCrunchDataset <- function(x, ...) {
  cat("---Crunch Automation command---\n")
  cat(make_query_text(x))
}

#' @export
compute.AutoReadyCrunchDataset <- function(x, name = NULL, ...) {
  query <- make_query_text(x)
  if (is.null(query)) return(x)
  
  out <- runCrunchAutomation(x, query)
  CrunchDataset(out) # TODO: Does this remove autoready class?
}


#' @export
categorical_array <- function(
  ..., 
  labels = NULL, 
  title = NULL, 
  description = NULL, 
  notes = NULL
) {
  sv_aliases <- list(...)
  if (is.data.frame(sv_aliases[[1]])) { # across gives a data.frame
    sv_aliases <- purrr::flatten(sv_aliases) 
  }
  sv_aliases <- lapply(sv_aliases, function(x) noquote(alias(x)))
  
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
    alias = "TKTKTKTK", # TODO: Needs to be replaced by LHS
    title = title,
    description = description,
    notes = notes
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

ca_optional <- function(label, item, indent = 0, newline = TRUE) {
  if (is.null(item)) return("")
  
  indent_mark <- paste(rep(" ", indent), collapse = "")
  newline_mark <- if (newline) paste0("\n", indent_mark) else " "
  item <- ca_quote_items(item)
  glue::glue(
    "{newline_mark}{item}"
  )
}

ca_quote_items <- function(items) {
  purrr::map_chr(items, function(item) {
    if (is.numeric(item) || inherits(item, "noquote")) {
      paste0(item)
    } else {
      paste0("\"", item, "\"")
    }
  })
}

ca_template <- function(...) {
  function(x) glue::glue_data(x, ...)
}
