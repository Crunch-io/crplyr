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
  current_steps$steps <- c(current_steps$steps, cmd)
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
  steps <- attr(x, "steps")
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
categorical_array <- function(..., labels, alias, title, description, notes) {
  sv_aliases <- list(...)
  if (is.data.frame(sv_aliases[[1]])) { # across gives a data.frame
    sv_aliases <- purrr::flatten(sv_aliases) 
  }
  
  cmd <- paste0(
    "CREATE CATEGORICAL ARRAY\n  ", 
    paste0(lapply(sv_aliases, function(x) attr(x, "alias")), collapse = ", "),
    "\n  LABELS ", paste0("\"", labels, "\"", collapse = ", "), "\n",
    "AS ", alias,
    if (!missing(title)) paste0("\nTITLE \"", title, "\""),
    if (!missing(description)) paste0("\nDESCRIPTION \"", description, "\""),
    if (!missing(notes)) paste0("\nNOTES \"", notes, "\""),
    ";"
  )
  
  add_crunch_auto_step(cmd)
}

  
