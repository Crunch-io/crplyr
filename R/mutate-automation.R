#' @export
as_crunch_auto_tbl <- function(x) {
  # TODO: cleanup this code
  shadow_var_tibble <- as_tibble(
    setNames(lapply(aliases(allVariables(x)),
                    function(alias) {
                      crunch_var <- x[[alias]]
                      out <- list()
                      
                      attributes(out) <- list(
                        alias = alias,
                        title = name(crunch_var),
                        description = description(crunch_var),
                        notes = notes(crunch_var),
                        type = type(crunch_var)
                      )
                      out
                    }
    ), aliases(allVariables(x)))
  )
  
  # TODO: could the `CrunchDataset` be the main object, or is
  # that not possible with S4 objects?
  out <- shadow_var_tibble
  attributes(out) <- c(
    attributes(out), 
    list(
      full_dataset = x,
      steps = list()
    )
  )
  class(out) <- c("crunch_auto_tbl", class(out))
  out
}

current_steps <- rlang::child_env(NULL)

add_crunch_auto_step <- function(cmd) {
  current_steps$steps <- c(current_steps$steps, list(cmd))
}


#' @export
mutate.crunch_auto_tbl <- function(.data, ...) {
  old_steps <- current_steps$steps
  current_steps$steps <- attr(.data, "steps")
  on.exit(current_steps$steps <- old_steps, add = TRUE)
  
  ignore <- NextMethod()
  out <- .data
  attr(out, "steps") <- current_steps$steps
  out
}

make_query_text <- function(x) {
  steps <- lapply(attr(x, "steps"), format_ca_auto)
  if (length(steps) == 0) return(NULL)  
  paste(steps, collapse = "\n\n")
}

#' @export
show_query.crunch_auto_tbl <- function(x, ...) {
  cat("---Crunch Automation command---\n")
  cat(make_query_text(x))
}

#' @export
compute.crunch_auto_tbl <- function(x, name = NULL, ...) {
  out_ds <- collect(x)
  # recalculate shadow tibble
  as_crunch_auto_tbl(out_ds)
}

#' @export
collect.crunch_auto_tbl <- function(x, ...) {
  query <- make_query_text(x)
  if (is.null(query)) return(x)
  ds <- attr(x, "full_dataset")
  
  runCrunchAutomation(ds, query)
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
  sv_aliases <- lapply(sv_aliases, function(x) noquote(attr(x, "alias")))
  
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
  
  add_crunch_auto_step(cmd)
  return(NULL)
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