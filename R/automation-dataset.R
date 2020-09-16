as_crunch_auto_tbl <- function(x) { # TODO: move to a S4 initializer?
  out <- AutomationCrunchDataset(x)
  out@commands <- list()
  out@var_df <- crunch_var_df_from_dataset(x)
  out
}

# A data.frame with list columns containing crunch variables,
# automation commands, placeholder variables (after a commands has been
# generated) or other objects when passed as crunch automation
# keywords. NB: `tibble` doesn't allow this data.frame because CrunchVars
# are not true "vectors" so be careful of manipulating this
as_crunch_var_df <- function(list) {
  structure(list, row.names = c(NA, -1L), class = c("crunch_var_df", "data.frame"))
}

# Create a crunch_var_df from a CrunchDataset
crunch_var_df_from_dataset <- function(vars) {
    all_aliases <- aliases(allVariables(vars))
    var_list <- map(all_aliases, function(alias) vars[[alias]])
    names(var_list) <- all_aliases
    
    as_crunch_var_df(var_list)
} 

# Create a crunch_var_df from a list of arguments to 
# a crplyr command
crunch_var_df_from_dots <- function(vars) {
  all_aliases <- map_chr(
    vars, 
    function(var) {
      # automation commands can create multiple aliases, so no alias method
      # for it.
      if (is_auto_cmd(var)) { 
        out <- aliases(var)
        if (length(out) > 1) {
          out <- paste0(out[1], "...", out[length(out)])
        }
        out
      } else if (is_var_or_placeholder(var)) {
        alias(var) 
      } else {
        as.character(var)
      }
    })
  var_list <- setNames(vars, all_aliases)
  
  as_crunch_var_df(var_list)
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
  if (!all(map_lgl(x, is_var_like))) {
    stop("All variables must be CrunchVariables")
  }
  out <- map(x, function(var) {
    if (is_var_or_placeholder(var)) {
      alias(var)
    } else if (is_auto_cmd(x)) {
      aliases(var)
    }
  })
  out <- flatten_chr(out)
})

setMethod("alias", "var_placeholder", function(object) {
  object$alias
})


# Internally we allow non crunchvars, we want to preserve the
# noquote status of non crunch vars, return all aliases from
# automation commands and backtick aliases with spaces
internal_aliases <- function(x) {
  out <- map(x, function(var) {
    if (is_var_or_placeholder(var)) {
      out <- alias(var)
      if (grepl("[[:space:]]", out)) out <- paste0("`", out, "`")
      list(noquote(out))
    } else if (is_auto_cmd(var)) {
      out <- aliases(var)
      out <- ifelse(grepl("[[:space:]]", out), paste0("`", out, "`"), out)
      lapply(out, noquote)
    } else {
      list(var)
    }
  })
  out <- unname(flatten(out))
  out
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
