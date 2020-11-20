read_excel_editor <- function(path) {
    raw <- try(readxl::read_excel(
        path = path,
        sheet = "Variables Editor",
        skip = 1,
        col_types = "list",
        col_names = c(
            "type", "folder", "orig_alias", "orig_child_alias", "alias", "child_alias", "title",
            "description", "notes", "orig_code", "code", "name", "missing", "selected", "value",
            "date"
        )
    ), silent = TRUE)
    if (inherits(raw, "try-error")) stop(paste0("Could not read data:\n", attr(raw, "message")))

    raw <- validate_header(raw)
    raw <- validate_cols(raw)

    out <- tidy_format(raw)
    out <- validate_as_whole(out)
    out
}

# Tidying ----
tidy_format <- function(raw) {
    # Remove empty rows
    combined <- dplyr::filter(raw, rowSums(dplyr::across(.fns = is.na)) != 1)

    # Delineate where the variables and metadata sections begin/end
    combined <- dplyr::mutate(
        combined,
        var_row = !is.na(.data$alias),
        subvar_row = !is.na(.data$child_alias),
        cat_row = !is.na(.data$orig_code),
        var_id = cumsum(.data$var_row),
        alias_fill = .data$alias,
        child_alias_fill = ifelse(var_row, "<NOT.A.SV>", child_alias),
        orig_alias_fill = ifelse(var_row, "<NOT.A.SV>", orig_alias),
        orig_child_alias_fill = ifelse(var_row, "<NOT.A.SV>", orig_child_alias)
    )

    combined <- tidyr::fill(combined, .data$child_alias_fill, .data$orig_alias_fill, .data$orig_child_alias_fill)
    combined <- dplyr::mutate(
        combined,
        dplyr::across(dplyr::ends_with("fill"), ~ifelse(. == "<NOT.A.SV>", NA, .))
    )

    var_info <- dplyr::filter(combined, .data$var_row)
    var_info <- dplyr::select(
        var_info,
        dplyr::one_of(c("var_id", "type", "folder", "orig_alias", "orig_child_alias", "alias", "title", "description", "notes"))
    )

    subvar_info <- dplyr::filter(combined, .data$subvar_row)
    subvar_info <- dplyr::select(
        subvar_info,
        dplyr::one_of(c("var_id", "type", "orig_alias", "orig_child_alias", "child_alias", "title", "description", "notes"))
    )
    subvar_info <- dplyr::nest_by(subvar_info, .data$var_id, .key = "subvars")

    cat_info <- dplyr::filter(combined, .data$cat_row)
    cat_info <- dplyr::select(
        cat_info,
        dplyr::one_of(c(
            "var_id", "child_alias_fill", "orig_code", "code",
            "name", "missing", "selected", "value", "date"
        ))
    )
    cat_info <- dplyr::rename_with(cat_info, ~gsub("_fill$", "", .))
    cat_info <- dplyr::nest_by(cat_info, .data$var_id, .key = "categories")


    tidy <- full_join(var_info, subvar_info, by = "var_id")
    tidy <- full_join(tidy, cat_info, by = "var_id")
    tidy
}

# Validation ----
validate_header <- function(df) {
    expected_header <- c(
        "Type", "Folder", "Original Alias", "Original Child Alias", "Alias", NA, "Title",
        "Description", "Notes", "Original Code", "Code", "Name", "Missing", "Selected", "Value",
        "Date"
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
    df <- validate_col(df, "description", validate_chr)
    df <- validate_col(df, "notes", validate_chr)
    df <- validate_col(df, "orig_code", validate_num)
    df <- validate_col(df, "code", validate_num)
    df <- validate_col(df, "name", validate_chr)
    df <- validate_col(df, "missing", validate_lgl)
    df <- validate_col(df, "selected", validate_lgl)
    df <- validate_col(df, "value", validate_num)
    df <- validate_col(df, "date", validate_date)

    col_errors <- attr(df, "column_errors")
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

    folder_regex <- stringr::regex("^(Root|Hidden|Secure)?(\\|.*)?$", ignore_case = TRUE)
    okay_dirs <- stringr::str_detect(out, folder_regex) | is.na(out)
    if (any(!okay_dirs)) {
        bad_dirs <- paste0("'", unique(out[!okay_dirs]), "'", collapse = ", ")
        stop(paste0(
            "Could not read data:\nFolder paths do not all start with '|',",
            "'Root|', 'Hidden|' or 'Secure|'\n",
            bad_dirs
        ), call. = FALSE)
    }
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
    out <- validate_unique_aliases(out)
    out <- validate_unique_titles(out)
    out <- validate_cats_on_right_vars(out)
    out <- validate_svs_on_right_vars(out)
    out <- validate_cats_on_right_subvars(out)
    out <- validate_cats(out)

    problems <- attr(out, "whole_problems")
    if (length(problems) > 0) {
        problems <- paste0(problems, collapse = "\n")
        stop(paste0(
            "Could not read data - validation problems:\n", problems
        ), call. = FALSE)
    }
    out
}

# Validate categories
validate_cats <- function(tidy_df) {
    tidy_df$categories <- purrr::map(
        tidy_df$categories,
        ~try(validate_single_var_cats(.), silent = TRUE)
    )

    failures <- dplyr::filter(tidy_df, purrr::map_lgl(.data$categories, ~inherits(., "try-error")))
    if (nrow(failures) > 0) {
        message <- paste(purrr::map2_chr(
            failures$alias,
            failures$categories,
            ~paste0("(", .x, ") ", attr(.y, "condition")$message)
        ), collapse = "\n")
        attr(tidy_df, "whole_problems") <- c(attr(tidy_df, "whole_problems"), message)
    }
    tidy_df
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

# Categories must have one metadata item per code in the output
# and unique names
validate_single_var_cats <- function(cats) {
    if (is.null(cats) || nrow(cats) == 0) return(cats)
    problems <- c()

    # orig_code is not missing
    missing_orig_code <- is.na(cats$orig_code)
    if (any(missing_orig_code)) {
        problems <- c(problems, "Some categories missing original code values.")
    }

    # category metadata are 1-1 with unique codes (and names are unique)
    cat_meta <- c("name", "missing", "selected", "value", "date")
    many_to_one <- dplyr::group_by(cats, .data$child_alias, .data$code)
    many_to_one <- dplyr::summarize_at(
        many_to_one,
        cat_meta,
        ~list(setdiff(unique(.), NA))
    )

    for (var in cat_meta) {
        many_to_one_failures <- dplyr::filter(many_to_one, lengths(.data[[var]]) > 1)
        if (nrow(many_to_one_failures) > 0) {
            many_to_one_failures$str <- purrr::map_chr(
                many_to_one_failures[[var]], ~paste0("'", ., "'", collapse = ", ")
            )
            bad_codes <- paste0(
                many_to_one_failures$code, " (", many_to_one_failures$str, ")", collapse = "; "
            )
            problems <- c(
                problems,
                paste0("Category IDs ", bad_codes, " have multiple values of ", var, " assigned.")
            )
        }
    }

    if (length(problems) > 0) {
        stop(paste0("- ", problems, collapse = "\n"), .call = FALSE)
    }

    # names are unique
    dups <- many_to_one
    dups$name <- unlist(dups$name)
    dups <- dplyr::group_by(dups, .data$child_alias, .data$name)
    dups <- dplyr::filter(dups, dplyr::n() > 1)
    if (nrow(dups) > 0) {
        dup_names <- paste0(unique(dups$name), collapse = ", ")
        stop(paste0("- Duplicated category names found: ", dup_names), call. = FALSE)
    }

    # Fill in metadata per code so it's easier to work with
    # (we've already checked that they're is only one value)
    out  <- dplyr::group_by(cats, .data$code)
    out <- dplyr::mutate_at(
        out,
        cat_meta,
        ~unique(.[!is.na(.)])[1]
    )
    dplyr::ungroup(out)
}

# Subvars only on CA/MR/NA (and all have at least one subvars)
validate_svs_on_right_vars <- function(tidy_df) {
    array_types <- c("categorical_array", "multiple_response", "numeric_array")
    checks <- dplyr::transmute(
        tidy_df,
        type = .data$type,
        alias = .data$alias,
        num_svs = purrr::map_dbl(.data$subvars, ~ifelse(is.null(.), 0, nrow(.))),
        sv_bad_types = purrr::map2_chr(.data$subvars, .data$type, function(svs, type) {
            if (type %in% c("categorical_array", "multiple_response")) {
                bad <- dplyr::filter(svs, .data$type != "categorical")
                if (nrow(bad) == 0) return(NA_character_)
            } else if (type == "numeric_array") {
                bad <- dplyr::filter(svs, .data$type != "numeric")
                if (nrow(bad) == 0) return(NA_character_)
            } else {
                return(NA_character_)
            }
            paste0(bad$child_alias, " (", bad$type, ")", collapse = ", ")
        })
    )

    nonarray_with_svs <- dplyr::filter(checks,  !.data$type %in% array_types & .data$num_svs > 0)
    if (nrow(nonarray_with_svs) > 0) {
        message <- paste0(
            "Non array variables cannot have subvariables defined: ",
            paste0("'", unique(nonarray_with_svs$alias), "'", collapse = ", ")
        )
        attr(tidy_df, "whole_problems") <- c(attr(tidy_df, "whole_problems"), message)
    }

    array_with_no_svs <- dplyr::filter(checks,  .data$type %in% array_types & .data$num_svs == 0)
    if (nrow(array_with_no_svs) > 0) {
        message <- paste0(
            "Array variables must have subvariables defined: ",
            paste0("'", unique(array_with_no_svs$alias), "'", collapse = ", ")
        )
        attr(tidy_df, "whole_problems") <- c(attr(tidy_df, "whole_problems"), message)
    }

    sv_bad_types <- dplyr::filter(checks, !is.na(.data$sv_bad_types))
    if (nrow(sv_bad_types > 0)) {
        message <- paste0(
            "Subvariables have wrong type for parent: ",
            paste0("'", unique(sv_bad_types$alias), "' - ", sv_bad_types$sv_bad_types, collapse = ", ")
        )
        attr(tidy_df, "whole_problems") <- c(attr(tidy_df, "whole_problems"), message)
    }

    tidy_df
}

# Aliases are unique
validate_unique_aliases <- function(tidy_df) {
    dups <- duplicated(tidy_df$alias)
    if (any(dups)) {
        dups_str <- paste0(
            "Duplicate aliases found: ",
            paste0("'", unique(tidy_df$alias[dups]), "'", collapse = ", ")
        )
        attr(tidy_df, "whole_problems") <- c(attr(tidy_df, "whole_problems"), dups_str)
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
