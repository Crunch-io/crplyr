read_excel_editor <- function(path) {
    raw <- try(readxl::read_excel(
        path = path,
        sheet = "Variables Editor",
        range = cellranger::cell_limits(c(2, 1), c(NA, 17)),
        col_types = "list",
        col_names = c(
            "type", "folder", "orig_alias", "orig_child_alias", "alias", "child_alias", "title",
            "subvar_label", "description", "notes", "orig_code", "code", "name", "missing",
            "selected", "value", "date"
        )
    ), silent = TRUE)
    if (inherits(raw, "try-error")) {
        stop(paste0("Error while reading excel file\n    ", raw))
    }

    raw <- validate_header(raw)
    raw <- validate_cols(raw)

    raw$orig_row <- seq_len(nrow(raw)) + 2 # include header
    out <- tidy_format(raw)
    out <- validate_as_whole(out)
    out
}

# Tidying ----
tidy_format <- function(raw) {
    # Remove empty rows
    combined <- remove_empty_rows(raw, -"orig_row")

    # Get the variable rows
    vars <- dplyr::select(
        combined, "type", "folder", "orig_alias", "orig_child_alias", "alias", "title",
        "description", "notes", "orig_row"
    )
    vars <- remove_empty_rows(vars, -c("orig_alias", "orig_child_alias", "orig_row"))
    vars <- validate_var_rows_are_consistent(vars)

    # Get the subvariable rows
    subvars <- dplyr::select(
        combined, "type", "orig_alias", "orig_child_alias", "alias", "child_alias",
        "subvar_label", "orig_row"
    )
    subvars <- remove_empty_rows(subvars, -c("orig_alias", "orig_child_alias", "orig_row"))
    subvars <- dplyr::group_by(subvars, .data$alias)
    subvars <- dplyr::filter(subvars, dplyr::n() > 1 | any(!is.na(.data$subvar_label)))
    subvars <- dplyr::ungroup(subvars)
    subvars <- validate_subvar_rows_are_consistent(subvars)

    # Get the category rows
    cats <- dplyr::select(
        combined,
        "orig_code", "alias", "code", "name", "missing", "selected", "value", "date", "orig_row"
    )
    # Get calculated subvar aliases from subvars
    cats <- dplyr::left_join(
        cats,
        dplyr::select(subvars, c("orig_row", "child_alias")),
        by = "orig_row"
    )
    cats <- dplyr::arrange(cats, .data$orig_row)
    cats <- tidyr::fill(cats, .data$alias)
    cats <- dplyr::group_by(cats, .data$alias)
    cats <- dplyr::mutate(cats, no_children = all(is.na(.data$child_alias)))
    cats <- dplyr::mutate(
        cats,
        child_alias = ifelse(.data$no_children, "<NONE>", .data$child_alias)
    )
    cats <- dplyr::ungroup(cats)
    cats <- dplyr::arrange(cats, .data$alias, .data$orig_row)
    cats <- tidyr::fill(cats, .data$child_alias)
    cats <- dplyr::mutate(
        cats,
        child_alias = ifelse(.data$child_alias == "<NONE>", NA, .data$child_alias)
    )
    cats <- dplyr::select(cats, -"no_children")
    cats <- dplyr::filter(cats, !is.na(.data$orig_code))
    cats <- validate_cat_rows_are_consistent(cats)


    errors <- c(attr(vars, "errors"), attr(subvars, "errors"), attr(cats, "errors"))
    if (length(errors) > 0) {
        stop(paste0(
            "Errors found in excel file structure:\n",
            paste0("- ", errors, collapse = "\n")
        ), call. = FALSE)
    }

    subvars <- dplyr::arrange(subvars, .data$orig_row)
    subvars <- dplyr::select(subvars, -"orig_row")
    subvars <- dplyr::group_by(subvars, .data$alias)
    subvars <- dplyr::group_nest(subvars, .key = "subvars")
    cats <- dplyr::arrange(cats, .data$orig_row)
    cats <- dplyr::select(cats, -"orig_row")
    cats <- dplyr::group_by(cats, .data$alias)
    cats <- dplyr::group_nest(cats, .key = "categories")

    tidy <- dplyr::full_join(vars, subvars, by = "alias")
    tidy <- dplyr::full_join(tidy, cats, by = "alias")
    tidy
}

validate_var_rows_are_consistent <- function(vars) {
    errors <- list()

    aliases_without_type <- dplyr::filter(
        vars,
        !is.na(.data$alias) & is.na(.data$type)
    )
    if (nrow(aliases_without_type) > 0) {
        bad_types <- paste0(
            "'", aliases_without_type$alias, "' (row ", aliases_without_type$orig_row, ")",
            collapse = ", "
        )
        errors <- c(
            errors,
            paste0("All variables and subvariables must have type: ", bad_types)
        )
    }

    out <- dplyr::group_by(vars, .data$alias)
    out <- dplyr::summarize(
        out,
        dplyr::across(c("orig_alias", "orig_child_alias"), ~.[1]),
        dplyr::across(c("orig_row"), ~min(.)),
        dplyr::across(c("type", "folder", "title", "description", "notes"), ~list(na.omit(unique(.)))),
        .groups = "drop"
    )
    out <- dplyr::arrange(out, .data[["orig_row"]])

    var_conflicting_meta <- purrr::map(
        c("type", "folder", "title", "description", "notes"),
        find_conflicting_var_meta,
        data = out
    )
    var_conflicting_meta <- var_conflicting_meta[lengths(var_conflicting_meta) > 0]
    errors <- c(errors, var_conflicting_meta)

    # Take the first value
    out <- dplyr::mutate(
        out, dplyr::across(
            c("type", "folder", "title", "description", "notes"),
            ~unlist(purrr::map(., ~.[1]))
        )
    )
    attr(out, "errors") <- errors
    out
}

validate_subvar_rows_are_consistent <- function(subvars) {
    errors <- list()

    missing_label <- dplyr::filter(subvars, is.na(subvar_label))
    if (nrow(missing_label) > 0) {
        aliases <- paste0(
            missing_label$alias,
            " (row: ", missing_label$orig_row, ")",
            collapse = ", "
        )

        errors <- c(
            errors,
            paste0(
                "All subvariables must have a label: ", aliases
            )
        )
    }

    duplicate_label <- dplyr::group_by(subvars, .data$alias, .data$subvar_label)
    duplicate_label <- dplyr::filter(duplicate_label, dplyr::n() > 1)
    if (nrow(duplicate_label) > 0) {
        aliases <- paste0(
            duplicate_label$alias, ".", duplicate_label$child_alias,
            "(", duplicate_label$subvar_label ,")",
            collapse = ", "
        )

        errors <- c(
            errors,
            paste0(
                "All subvariables must have unique labels: ", aliases
            )
        )
    }
    out <- dplyr::group_by(subvars, .data$alias)
    out <- dplyr::mutate(
        out,
        child_alias = ifelse(
            is.na(.data$child_alias),
            paste0("<sv", dplyr::row_number(), ">"),
            .data$child_alias
        )
    )
    out <- ungroup(out)
    attr(out, "errors") <- errors
    out
}

validate_cat_rows_are_consistent <- function(cats) {
    errors <- list()

    missing_code <- dplyr::filter(cats, is.na(.data$code))
    if (nrow(missing_code) > 0) {
        info <- paste0(
            "Category codes cannot be blank: ", paste(missing_code$orig_row, collapse = ", ")
        )
        errors <- c(errors, info)
    }
    missing_names <- dplyr::filter(cats, is.na(.data$code))
    if (nrow(missing_names) > 0) {
        info <- paste0(
            "Category names cannot be blank: ", paste(missing_names$orig_row, collapse = ", ")
        )
        errors <- c(errors, info)
    }

    out <- dplyr::group_by(cats, .data$alias, .data$child_alias, .data$code)
    out <- dplyr::summarize(
        out,
        dplyr::across(c("orig_code", "name", "missing", "selected", "value", "date"), ~list(na.omit(unique(.)))),
        dplyr::across(c("orig_row"), min),
        .groups = "drop"
    )

    conflicting_meta <- purrr::map(
        c("name", "missing", "selected", "value", "date"),
        find_conflicting_var_meta, data = out
    )
    conflicting_meta <- conflicting_meta[lengths(conflicting_meta) > 0]
    errors <- c(errors, conflicting_meta)

    out <- dplyr::mutate(
        out, dplyr::across(
            c("name", "missing", "selected", "value", "date"),
            ~unlist(purrr::map(., ~.[1]))
        )
    )
    out

    attr(out, "errors") <- errors
    out
}

remove_empty_rows <- function(data, .cols = dplyr::everything()) {
    dplyr::filter(data, rowSums(!dplyr::across(.cols = {{.cols}}, .fns = is.na)) > 0)
}

find_conflicting_var_meta <- function(data, type) {
    conflicts <- dplyr::filter(data, lengths(.data[[type]]) > 1)

    if (nrow(conflicts) == 0) return(NULL)
    conflict_info <- paste0(
        "'", conflicts$alias, "' (",
        purrr::map_chr(conflicts[[type]], ~paste0(., collapse = ", ")), ")",
        collapse = "; "
    )
    paste0(
        "Metadata of type '", type,
        "' is not consistent for some variables: ",
        conflict_info
    )
}

# Validation ----
validate_header <- function(df) {
    expected_header <- c(
        "Type", "Folder", "Original Alias", "Original Child Alias", "Alias", "Child Alias", "Title",
        "Subvariable Label", "Description", "Notes", "Original Code", "Code", "Name", "Missing",
        "Selected", "Value", "Date"
    )
    header <- purrr::map_chr(df[1, ], ~as.character(.[[1]]))
    comparison <- waldo::compare(unname(header), expected_header)
    if (length(comparison) > 0) {
        print(comparison)
        stop("Could not read data:\nheader does not contain expected column names")
    }
    # Remove header column now that we've validated it
    df[-1, ]
}

# Column validators ----
validate_cols <- function(df) {
    df <- validate_col(df, "type", validate_type)
    df <- validate_col(df, "folder", validate_folder)
    df <- validate_col(df, "orig_alias", validate_chr)
    df <- validate_col(df, "orig_child_alias", validate_chr)
    df <- validate_col(df, "alias", validate_chr)
    df <- validate_col(df, "child_alias", validate_chr)
    df <- validate_col(df, "title", validate_chr)
    df <- validate_col(df, "subvar_label", validate_chr)
    df <- validate_col(df, "description", validate_chr)
    df <- validate_col(df, "notes", validate_chr)
    df <- validate_col(df, "orig_code", validate_num)
    df <- validate_col(df, "code", validate_num)
    df <- validate_col(df, "name", validate_chr)
    df <- validate_col(df, "missing", validate_lgl)
    df <- validate_col(df, "selected", validate_lgl)
    df <- validate_col(df, "value", validate_num)
    df <- validate_col(df, "date", validate_date)

    col_errors <- attr(df, "col_errors")
    if (length(col_errors) > 0) {
        col_errors <- paste(col_errors, collapse = "\n - ")
        stop("Could not read data:\nColumn errors:\n", col_errors)
    }
    df
}

validate_col <- function(df, col_name, validate_func, ...) {
    new_col <- try(validate_func(df[[col_name]], ...), silent = TRUE)
    if (inherits(new_col, "try-error")) {
        message <- paste0(col_name, ": ", attr(new_col, "condition"))
        attr(df, "col_errors") <- c(attr(df, "col_errors"), message)
        return(df)
    }
    df[[col_name]] <- new_col
    df
}

is_date <- function(x) {
    inherits(x, "POSIXt")
}

posix_to_crunch_date <- function(x) {
    paste0(
        lubridate::year(.), "-", lubridate::month(.), "-", lubridate::day(.)
    )
}

validate_chr <- function(column) {
    out <- purrr::map_chr(column, as.character)
    # Excel doesn't make it clear to user difference between empty cell and ""
    # so we don't depend on there being a difference
    out[out == ""] <- NA_character_
    out
}

validate_date <- function(column) {
    out <- purrr::modify_if(column, is_date, posix_to_crunch_date)
    out <- validate_chr(out)

    date_regex <- stringr::regex(
        paste0(
            "^[:digit:]{4}$|",
            "^[:digit:]{4}-[:digit:]{2}$|",
            "^[:digit:]{4}-W[:digit:]{2}$|",
            "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"
        )
    )
    okay_dates <- stringr::str_detect(out, date_regex) | is.na(out)
    if (any(!okay_dates)) {
        bad_dates <- paste0("'", unique(out[!okay_dates]), "'", collapse = ", ")
        stop(paste0(
            "Dates are not in expected format: ",
            bad_dates
        ), call. = FALSE)
    }
    out
}

validate_folder <- function(column) {
    out <- validate_chr(column)

    # Add pipe to beginning of folders specified lazily
    folder_regex <- stringr::regex("^(Root|Hidden|Secure)?(\\|.*)?$", ignore_case = TRUE)
    need_leading_pipe <- !stringr::str_detect(out, folder_regex) & !is.na(out)
    out[need_leading_pipe] <- paste0("|", out[need_leading_pipe])

    out
}

validate_lgl <- function(column) {
    out <- validate_chr(column)
    okay_lgl_values <- out %in% c("X", "x") | is.na(out)
    if (any(!okay_lgl_values)) {
        bad_lgl_values <- paste("'", unique(out[!okay_lgl_values]), "'", collapse = ", ")
        stop(paste0(
            "Could not read data:\nExpected either 'X' or 'x', but found: ",
            bad_lgl_values
        ), call. = FALSE)
    }
    out %in% c("X", "x")
}

validate_num <- function(column) {
    rows_are_dates <- purrr::map_lgl(column, is_date)
    if (any(rows_are_dates)) {
        bad_rows <- which(rows_are_dates) + 2 # include heaer cols
        stop(paste0(
            "values expected to be number but are date in rows: ",
            paste(bad_rows, collapse = ", ")
        ), call. = FALSE)
    }

    out <- purrr::map(column, ~try(as.numeric(.), silent = TRUE))
    rows_are_not_numeric <- purrr::map_lgl(out, ~inherits(., "try-error"))
    if (any(rows_are_not_numeric)) {
        bad_rows <- which(rows_are_dates) + 2 # include heaer cols
        stop(paste0(
            "values expected to be number but could not be converted in rows: ",
            paste(bad_rows, collapse = ", ")
        ), call. = FALSE)
    }

    purrr::flatten_dbl(out)
}

validate_type <- function(column) {
    out <- validate_chr(column)

    expected_types <- c(
        "categorical", "categorical_array", "datetime", "multiple_response", "numeric",
        "numeric_array", "text"
    )
    type_is_expected <- is.na(out) | out %in% expected_types
    if (any(!type_is_expected)) {
        bad_types <- paste0("'", unique(column[!type_is_expected]), "'", collapse = ", ")
        stop("Unexpected types found: ", bad_types, call. = FALSE)
    }
    out
}

# Post tidying validation ----
validate_as_whole <- function(tidy_df) {
    out <- tidy_df
    out <- validate_unique_titles(out)
    out <- validate_cats_on_right_vars(out)
    out <- validate_cats_on_right_subvars(out)

    problems <- attr(out, "whole_problems")
    if (length(problems) > 0) {
        problems <- paste0(problems, collapse = "\n")
        stop(paste0(
            "Could not read data - validation problems:\n", problems
        ), call. = FALSE)
    }
    out
}

# categories only on categorical/categorical_array vars (and all categorical/cateogrical_array
# have at least one category)
validate_cats_on_right_vars <- function(tidy_df) {
    cat_types <- c("categorical", "categorical_array", "multiple_response")
    checks <- dplyr::transmute(
        tidy_df,
        type = .data$type,
        alias = .data$alias,
        num_cats = purrr::map_dbl(.data$categories, ~ifelse(is.null(.), 0, nrow(.)))
    )

    noncat_with_cats <- dplyr::filter(checks,  !.data$type %in% cat_types & .data$num_cats > 0)
    cat_with_no_cats <- dplyr::filter(checks,  .data$type %in% cat_types & .data$num_cats == 0)
    if (nrow(noncat_with_cats) > 0) {
        message <- paste0(
            "Non categorical variables cannot have categories defined: ",
            paste0("'", unique(noncat_with_cats$alias), "'", collapse = ", ")
        )
        attr(tidy_df, "whole_problems") <- c(attr(tidy_df, "whole_problems"), message)
    }
    if (nrow(cat_with_no_cats) > 0) {
        message <- paste0(
            "Categorical variables must have categories defined: ",
            paste0("'", unique(cat_with_no_cats$alias), "'", collapse = ", ")
        )
        attr(tidy_df, "whole_problems") <- c(attr(tidy_df, "whole_problems"), message)
    }

    tidy_df
}

# categories get assigned to subvars, but only valid if either all svs have cats
# or just one does (in which case cats apply to all)
validate_cats_on_right_subvars <- function(tidy_df) {
    checks <- tidy_df
    checks <- dplyr::filter(
        checks,
        lengths(.data$categories) > 0 & lengths(.data$subvars) > 0
    )
    checks$num_svs_with_cats <- purrr::map_dbl(
        checks$categories,
        ~length(unique(.$child_alias[!is.na(.$child_alias)]))
    )
    checks$num_svs <- purrr::map_dbl(
        checks$subvars,
        ~length(unique(.$child_alias)[!is.na(.$child_alias)])
    )

    failed_checks <- dplyr::filter(
        checks,
        .data$num_svs_with_cats > 1 & .data$num_svs_with_cats != .data$num_svs
    )

    if (nrow(failed_checks) > 0) {
        message <- paste0(
            "Subvariables must be defined for either a single subvariable (and will apply for ",
            "all of them) or all subvariables, but these variables have categories defined only ",
            "for some: ", paste0("'", failed_checks$alias, "'", collapse = ", ")
        )
        attr(tidy_df, "whole_problems") <- c(attr(tidy_df, "whole_problems"), message)
    }

    cats_apply_for_all <- dplyr::filter(checks, .data$num_svs_with_cats == 1)
    for (alias in cats_apply_for_all$alias) {
        tidy_df$categories[[which(tidy_df$alias == alias)]]$child_alias <- NA
    }

    tidy_df
}

# Titles are unique
validate_unique_titles <- function(tidy_df) {
    dups <- duplicated(tidy_df$title)
    if (any(dups)) {
        dups_str <- paste0(
            "Duplicate titles found: ",
            paste0("'", unique(tidy_df$title[dups]), "'", collapse = ", ")
        )
        attr(tidy_df, "whole_problems") <- c(attr(tidy_df, "whole_problems"), dups_str)
    }

    missing_title <- is.na(tidy_df$title)
    if (any(missing_title)) {
        missing_str <- paste0(
            "Variables without titles found: ",
            paste0("'", unique(tidy_df$alias[missing_title]), "'", collapse = ", ")
        )
        attr(tidy_df, "whole_problems") <- c(attr(tidy_df, "whole_problems"), missing_str)
    }
    tidy_df
}
