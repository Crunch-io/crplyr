as_crunch_auto_tbl <- function(x) { # TODO: move to a S4 initializer?
  out <- AutomationCrunchDataset(x)
  out@steps <- list()
  out@var_df <- as_crunch_var_df(x)
  out
}

# A data.frame with list columns containing crunch variables
# `tibble` doesn't allow this data.frame because CrunchVars
# are not true "vectors" so be careful of manipulating this
#' @importFrom purrr map map_chr
as_crunch_var_df <- function(vars) {
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
    all_aliases <- map_chr(vars, function(var) if (is_var_or_similar(var)) alias(var) else as.character(var))
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
  if (!all(map_lgl(x, is_var_or_similar))) stop("All variables must be CrunchVariables")
  names(x)
})

setMethod("alias", "crunch_auto_cmd", function(object) {
  object[[1]]$get_aliases(object[[1]]$data)
})

setMethod("alias", "var_placeholder", function(object) {
  object$alias
})


# Internally we allow non crunchvars, we want to preserve the
# noquote status of non crunch vars, as well as backtick aliases
# with spaces
internal_aliases <- function(x) {
  out <- map(x, function(var) {
    if (is_var_or_similar(var)) {
      out <- alias(var)
      if (grepl("[[:space:]]", out)) out <- paste0("`", out, "`")
      noquote(out)
    } else {
      var
    }
  })
  unname(out)
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
