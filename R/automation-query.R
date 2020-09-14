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
