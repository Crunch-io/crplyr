#' @importFrom glue glue glue_collapse
ca_list_to_text <- function(
  pre_text = "",
  items,
  after_text = "",
  sep = ", ",
  sep_newline = NULL,
  start_newline = TRUE,
  line_wrap = 100,
  indent = 0,
  item_indent = indent + 2
) {
  if (is.null(items) || length(items) == 0) return("")

  items <- ca_quote_items(items)

  if (is.null(sep_newline)) {
    num_chars <- nchar(items)
    if (sum(num_chars) > line_wrap - indent) {
      sep_newline <- TRUE
    } else {
      sep_newline <- FALSE
    }
  }

  indent_mark <- paste(rep(" ", indent), collapse = "")
  item_indent_mark <- paste(rep(" ", item_indent), collapse = "")
  after_pre_text <- if (sep_newline) paste0('\n', item_indent_mark) else ' '
  before_after_text <- if (sep_newline) paste0('\n', indent_mark) else ' '
  collapse_mark <- if (sep_newline) paste0(sep, "\n", item_indent_mark) else sep
  if (start_newline) begin <- "\n" else begin <- ""

  if (pre_text == "") {
    formatted_pre_text <- indent_mark
  } else {
    formatted_pre_text <- glue("{indent_mark}{pre_text}{after_pre_text}")
  }

  if (after_text == "") {
    formatted_after_text <- ""
  } else {
    formatted_after_text <- glue("{before_after_text}{after_text}")
  }

  item_text <- glue_collapse(
    items,
    sep = collapse_mark
  )
  glue("{begin}{formatted_pre_text}{item_text}{formatted_after_text}")
}

#' @importFrom purrr map_chr
ca_quote_items <- function(items) {
  map_chr(items, function(item) {
    if (is.numeric(item) || inherits(items, "noquote") || inherits(item, "noquote")) {
      paste0(item)
    } else {
      paste0("\"", item, "\"")
    }
  })
}

ca_text_if <- function(logical, text) {
  if (logical) text else ""
}

#' @importFrom glue glue_data
#' @importFrom rlang ns_env
ca_template <- function(...) {
  function(x) glue_data(x, ..., .envir = ns_env("crplyr"))
}

#' Crunch Automation Syntax Helpers
#'
#' Functions that help create special keywords for use in Crunch Automation commands.
#'
#' The `ca` object contains the following objects and functions:
#' - `category(label = NA, code = NA, missing = FALSE, ...)`: A function for creating a
#'    category label which has a string label, an integer code, and a logical indicating
#'    whether the category is a missing category. Used within other functions that may use
#'    other arguments in `...`.
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
  after = function(alias) noquote(paste0("AFTER "), alias),
  before = function(alias) noquote(paste0("BEFORE "), alias),
  category = function(label = NA, code = NA, missing = NA, ...) {
    tibble(label = label, code = code, missing = missing, ...)
  },
  copy = noquote("COPY"),
  dots = function(x, y) noquote(paste0(alias(x), "...", alias(y))),
  like = function(x) noquote(paste0("LIKE(\"", x, "\")")),
  max = function() noquote("MAX"),
  min = function() noquote("MIN"),
  regex = function(x) noquote(paste0("REGEX(\"", x, "\")")),
  regexp = function(x) noquote(paste0("REGEXP(\"", x, "\")")),
  use_descriptions = noquote("USE DESCRIPTIONS"),
  use_titles = noquote("USE TITLES"),
  keyword = noquote
)

ca_cat_df_to_text <- function(df, use_from = FALSE, after_from = " ") {
  if (nrow(df) == 0) return(NULL)
  if (!"code" %in% names(df)) df$code <- NA
  if (!"missing" %in% names(df)) df$missing <- NA
  if (!"from" %in% names(df)) df$from <- NA
  pmap(df, ca_cat_to_text, use_from = use_from, after_from = after_from)
}

ca_cat_to_text <- function(label, code, missing, ..., use_from = FALSE, after_from = " ") {
  dots <- list(...)
  if (use_from) {
    from_txt <- paste0(ca_quote_items(dots$from), after_from)
  } else {
    from_txt <- ""
  }

  if (is.na(label)) return(from_txt)
  code_text <- if (!is.na(code)) paste0(" CODE ", code) else ""
  missing_text <- if (isTRUE(missing)) " MISSING" else ""
  noquote(paste0(from_txt, ca_quote_items(label), code_text, missing_text))
}

#' @importFrom purrr pmap
#' @importFrom glue glue
cat_cases_df_to_text <- function(df) {
  if (!"code" %in% names(df)) df$code <- NA
  if (!"missing" %in% names(df)) df$missing <- NA
  if (!"variable" %in% names(df)) df$variable <- NA

  pmap(df, function(expr, label, code, missing, variable, ...) {
    if (expr == "TRUE") {
      expr <- "ELSE "
    } else {
      expr <- glue("WHEN {expr} THEN ")
    }

    if (!is.na(label) & !is.na(variable)) {
      stop("Cannot define both a category label and a variable to assign to.")
    } else if (is.na(label) && is.na(variable)) {
      rhs <- "INTO NULL"
    } else if (!is.na(label)) {
      ca_cat_to_text(label, code, missing)
    } else {
      rhs <- paste0("VARIABLE ", alias(rhs))
    }

    noquote(paste0(expr, rhs))
  })
}
