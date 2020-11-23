find_changes <- function(old, new) {
    new <- flat_vars_format(new)
    old <- flat_vars_format(old)

    validate_orig_alias(old, new)

    # Schema changes require changing in old too so that
    # we can build other variables off the new schema
    renames <- find_renames(new)
    old <- apply_renames(old, renames)
    new <- apply_renames(new, renames)

    validate_orig_codes(old, new)
    recodes <- find_recodes(old, new)
    new <- apply_recodes(new, recodes)
    old <- apply_recodes(old, recodes)

    validate_no_type_changes(old, new)

    # Now find existing variables that need to change
    var_meta_changes <- find_var_meta_changes(old, new)
    subvar_meta_changes <- find_subvar_meta_changes(old, new)
    cat_meta_changes <- find_cat_meta_changes(old, new)

    # And variables that need to be created
    validate_creations_only_array(new)
    array_creations <- find_array_creations(old, new)

    # And the folder structure
    folder_structure <- find_folder_structure(old, new)

    # Return commands in a list
    alter_commands <- dplyr::bind_rows(var_meta_changes, subvar_meta_changes, cat_meta_changes)
    alter_commands <- dplyr::arrange(alter_commands, .data$alias)

    list(
        schema_commands = dplyr::bind_rows(renames, recodes),
        alter_commands = alter_commands,
        create_commands = array_creations,
        organize_commands = folder_structure
    )

}

flat_vars_format <- function(vars) {
    if (!"var_id" %in% names(vars)) vars$var_id <- seq_len(nrow(vars))
    vars$subvars <- purrr::map2(vars$subvars, vars$categories, function(svs, cats) {
        if (is.null(svs) || nrow(svs) == 0) return(NULL)
        if (is.null(cats) || nrow(cats) == 0) {
            out <- svs
        } else if (all(is.na(cats$child_alias))) {
            cats$child_alias <- NULL
            svs$categories <- rep(list(cats), nrow(svs))
            out <- svs
        } else {
            cats <- tidyr::nest(cats, categories = -.data$child_alias)
            out <- dplyr::full_join(svs, cats, by = "child_alias")
            out$categories <- purrr::map(out$categories, identity) # help dplyr figure out types
        }
        dplyr::rename(out, "alias" = "child_alias")
    })

    vars$categories <- purrr::map2(vars$subvars, vars$categories, function(svs, cats) {
        if (is.null(cats) || nrow(cats) == 0) return(NULL)
        if (!(is.null(svs) || nrow(svs) == 0)) return(NULL)
        dplyr::select(cats, -"child_alias")
    })

    vars$new_var <- is.na(vars$orig_alias)

    subvars <- dplyr::select(vars, "var_id", "new_var", "alias", "subvars")
    subvars <- dplyr::filter(subvars, lengths(.data$subvars) > 0)
    subvars <- dplyr::rename(subvars, parent_alias = "alias")
    subvars <- tidyr::unnest(subvars, .data$subvars)
    subvars <- dplyr::mutate(subvars, sv_id = dplyr::row_number())

    vars <- dplyr::select(vars, -"subvars")
    vars <- dplyr::mutate(vars, parent_alias = NA_character_, sv_id = 0)

    out <- dplyr::bind_rows(vars, subvars)
    out <- dplyr::arrange(out, .data$var_id, .data$sv_id)
    out <- dplyr::mutate(out, orig_alias = ifelse(is.na(.data$orig_child_alias), .data$orig_alias, .data$orig_child_alias))
    out <- dplyr::select(out, "new_var", "type", "folder", "parent_alias", "orig_alias", "alias", "title", "description", "notes", "categories")
    out
}

validate_orig_alias <- function(old, new) {
    # Only checking for top level variables
    new <- dplyr::filter(new, !.data$new_var & is.na(.data$parent_alias))
    old <- dplyr::filter(old, !.data$new_var & is.na(.data$parent_alias))

    dup_vars <- new$orig_alias[duplicated(new$orig_alias)]
    if (length(dup_vars) > 0) {
        stop(paste0(
            "Original aliases can only be used once, but some have duplicates: ",
            paste0("'",  dup_vars, "'", collapse = ", ")
        ))
    }

    unknown_vars <- setdiff(old$orig_alias, c(new$orig_alias, NA))
    if (length(unknown_vars) > 0) {
        stop(paste0(
            "Original aliases must be in new schema, but could not find: ",
            paste0("'",  unknown_vars, "'", collapse = ", ")
        ))
    }

    missing_vars <- setdiff(new$orig_alias, old$orig_alias)
    if (length(missing_vars) > 0) {
        stop(paste0(
            "Original aliases must be used once, but some are missing: ",
            paste0("'",  missing_vars, "'", collapse = ", ")
        ))
    }
}

find_renames <- function(new) {
    # Don't count new variables as renames
    renames <- dplyr::filter(new, !.data$new_var & .data$orig_alias != .data$alias)

    if (nrow(renames) == 0) return(NULL)

    if (any(!is.na(renames$parent_alias))) {
        renamed_children <- renames$alias[is.na(renames$parent_alias)]
        stop(paste0(
            "Cannot rename subvariables: ", paste0("'", renamed_children, "'", collapse = ", ")
        ), call. = FALSE)
    }

    out <- dplyr::tibble(
        alias = renames$orig_alias,
        type = "rename",
        from = renames$orig_alias,
        to = renames$alias
    )
    tidyr::nest(out, details = -c(.data$alias, .data$type))
}

apply_renames <- function(vars, renames) {
    if (is.null(renames) || nrow(renames) == 0) return(vars)
    renames <- dplyr::ungroup(tidyr::unnest(renames, .data$details))

    # rename variables
    renames <- dplyr::select(renames, "orig_alias" = "from", "to")
    out <- dplyr::left_join(vars, renames, by = "orig_alias")
    out <- dplyr::mutate(out, orig_alias = ifelse(is.na(.data$to), .data$orig_alias, .data$to))
    out$to <- NULL

    # rename subvariables
    renames <- dplyr::select(renames, "parent_alias" = "orig_alias", "to")
    out <- dplyr::left_join(out, renames, by = "parent_alias")
    out <- dplyr::mutate(out, parent_alias = ifelse(is.na(.data$to), .data$parent_alias, .data$to))
    out$to <- NULL

    out
}

validate_no_type_changes <- function(old, new) {
    old_types <- dplyr::select(old, "orig_alias", "old_type" = "type")
    new_types <- dplyr::filter(new, !.data$new_var)
    new_types <- dplyr::select(new_types, "orig_alias", "new_type" = "type")

    checks <- dplyr::inner_join(old_types, new_types, by = "orig_alias")
    checks <- dplyr::filter(checks, .data$new_type != .data$old_type)
    if (nrow(checks) > 0) {
        bad_vars <- paste0("'", checks$old_alias, "'")
        stop(paste0(
            "Cannot change variable type, but some types don't match data: ", bad_vars
        ), call. = FALSE)
    }
}

validate_orig_codes <- function(old, new) {
    # Will check codes for new variables later (if they're based on a var that's recode,
    # they need to start from the new values, not the old ones)
    new_recodes <- dplyr::filter(new, !.data$new_var)
    new_recodes <- dplyr::select(new_recodes, "alias", new_categories = "categories")

    old_recodes <- dplyr::select(old, "alias", old_categories = "categories")

    merged <- dplyr::full_join(new_recodes, old_recodes, by = "alias")

    merged$info <- purrr::map2_chr(
        merged$old_categories, merged$new_categories, function(old_cats, new_cats) {
            if (!dplyr::setequal(old_cats$orig_code, new_cats$orig_code)) {
                paste0(
                    "old: ", paste0(old_cats$orig_code, collapse = ", "),
                    " vs new: ", paste0(new_cats$orig_code, collapse = ", ")
                )
            } else {
                return(NA_character_)
            }
        }
    )

    mismatched_cats <- dplyr::filter(merged, .data$info > 0)
    if (nrow(mismatched_cats) > 0) {
        info <- paste0(
            "'", mismatched_cats$alias, "' (", paste0(mismatched_cats$info), ")", collapse = "; "
        )
        stop(paste0("Mismatched category codes in new schema vs dataset: ", info))
    }
}

find_recodes <- function(old, new) {
    # Will check codes for new variables later (if they're based on a var that's recode,
    # they need to start from the new values, not the old ones)
    new <- dplyr::filter(new, !.data$new_var)

    new_recodes <- dplyr::filter(new, lengths(.data$categories) > 0)
    new_recodes <- dplyr::select(new_recodes, "alias", "categories")
    new_recodes <- tidyr::unnest(new_recodes, .data$categories)
    new_recodes <- dplyr::filter(new_recodes, .data$orig_code != .data$code)

    new_recode_vars <- dplyr::semi_join(new, new_recodes, by = "alias")
    new_recode_vars <- dplyr::select(new_recode_vars, "alias", "parent_alias", new_categories = "categories")
    old_recode_vars <- dplyr::semi_join(old, new_recodes, by = "alias")
    old_recode_vars <- dplyr::select(old_recode_vars, "alias", old_categories = "categories")

    # Check that all child variables have the same catgories
    child_recodes <- dplyr::semi_join(new, new_recodes, by = "alias")
    child_recodes <- dplyr::select(child_recodes, "alias", "parent_alias", "categories")
    all_children <- dplyr::filter(
        new,
        !is.na(.data$parent_alias) & .data$parent_alias %in% child_recodes$parent_alias
    )
    all_children <- dplyr::select(all_children, "alias", "parent_alias")
    child_recodes <- dplyr::left_join(all_children, child_recodes, by = c("alias", "parent_alias"))

    if (nrow(child_recodes) > 0) {
        child_checks <- dplyr::group_by(child_recodes, .data$parent_alias)
        child_checks <- dplyr::summarize(
            child_checks,
            bad = !all(purrr::map_lgl(.data$categories, y = .data$categories[[1]], identical))
        )
        child_checks <- dplyr::filter(child_checks, .data$bad)

        if (nrow(child_checks) > 0) {
            stop(paste0(
                "Subvariables of existing arrays must all have the same categories: ",
                paste0("'", child_checks$parent_alias, "'", collapse = ", ")
            ))
        }
    }

    out <- dplyr::full_join(new_recode_vars, old_recode_vars, by = "alias")
    if (nrow(out) == 0) return(
        dplyr::tibble(type = character(0), alias = character(0), details = list())
    )
    out <- dplyr::mutate(out, parent_alias = ifelse(is.na(.data$parent_alias), .data$alias, .data$parent_alias))
    out <- dplyr::group_by(out, .data$parent_alias)
    out <- dplyr::summarize(
        out,
        child_aliases = list(.data$alias),
        new_categories = list(new_categories[[1]]),
        old_categories = list(old_categories[[1]]),
        .groups = "drop"
    )
    out <- dplyr::mutate(out, type = "recode")
    out <- dplyr::rename(out, alias = "parent_alias")
    out <- tidyr::nest(out, details = -c(.data$type, .data$alias))
    out
}

apply_recodes <- function(vars, recodes) {
    rcs <- tidyr::unnest(recodes, .data$details)

    for (recode_num in seq_len(nrow(rcs))) {
        needs_recode <- vars$alias %in% rcs$child_aliases[[recode_num]] & !vars$new_var
        new_cats <- rcs$new_categories[[recode_num]]
        new_cats$orig_code <- new_cats$code
        new_cats <- dplyr::distinct(new_cats, .data$orig_code, .keep_all = TRUE)
        vars$categories[needs_recode] <- rep(list(new_cats), sum(needs_recode))
    }
    vars
}

find_var_meta_changes <- function(old, new) {
    # Only interested in existing parent variables for now
    new_vars <- dplyr::filter(new, !.data$new_var & is.na(.data$parent_alias))
    new_vars <- dplyr::select(new_vars, "alias", "title", "description", "notes")
    new_vars <- dplyr::mutate(new_vars, dplyr::across(.fns = ~ifelse(is.na(.), "", .)))
    new_vars <- tidyr::pivot_longer(new_vars, -"alias", names_to = "attribute", values_to = "new")

    old_vars <- dplyr::select(old, "alias", "title", "description", "notes")
    old_vars <- dplyr::mutate(old_vars, dplyr::across(.fns = ~ifelse(is.na(.), "", .)))
    old_vars <- tidyr::pivot_longer(old_vars, -"alias", names_to = "attribute", values_to = "old")

    out <- dplyr::left_join(new_vars, old_vars, by = c("alias", "attribute"))
    out <- dplyr::filter(out, .data$new != .data$old)
    out$type <- "var_meta_change"
    tidyr::nest(out, details = -c(.data$type, .data$alias))
}

find_subvar_meta_changes <- function(old, new) {
    new_subvars <- dplyr::filter(new, !.data$new_var & !is.na(.data$parent_alias))
    old_subvars <- dplyr::filter(old, !.data$new_var & !is.na(.data$parent_alias))

    # subvars as a set (adding & removing = error for now)
    new_sv_set <- dplyr::group_by(new_subvars, .data$parent_alias)
    new_sv_set <- dplyr::summarize(new_sv_set, new_aliases = list(.data$alias), .groups = "drop")
    old_sv_set <- dplyr::group_by(old_subvars, .data$parent_alias)
    old_sv_set <- dplyr::summarize(old_sv_set, old_aliases = list(.data$alias), .groups = "drop")
    sv_set <- dplyr::full_join(new_sv_set, old_sv_set, by = "parent_alias")

    sv_set$set_equal <- purrr::map2_lgl(
        sv_set$old_aliases, sv_set$new_aliases, dplyr::setequal
    )
    bad_set <- dplyr::filter(sv_set, !.data$set_equal)

    if (nrow(bad_set) > 0) {
        stop(paste0(
            "Cannot add or remove subvariables: ",
            paste0("'", bad_set$parent_alias, "'", collapse = ", ")
        ))
    }

    # subvar order (must be in the same order)
    sv_set$set_equal <- purrr::map2_lgl(
        sv_set$old_aliases, sv_set$new_aliases, identical
    )
    bad_set <- dplyr::filter(sv_set, !.data$set_equal)

    if (nrow(bad_set) > 0) {
        stop(paste0(
            "Cannot change subvariable order: ",
            paste0("'", bad_set$parent_alias, "'", collapse = ", ")
        ))
    }

    # subvar title/description/notes
    new_subvars <- dplyr::select(new_subvars, "alias", "title", "description", "notes")
    new_subvars <- dplyr::mutate(new_subvars, dplyr::across(.fns = ~ifelse(is.na(.), "", .)))
    new_subvars <- tidyr::pivot_longer(new_subvars, -"alias", names_to = "attribute", values_to = "new")

    old_subvars <- dplyr::select(old_subvars, "alias", "title", "description", "notes")
    old_subvars <- dplyr::mutate(old_subvars, dplyr::across(.fns = ~ifelse(is.na(.), "", .)))
    old_subvars <- tidyr::pivot_longer(old_subvars, -"alias", names_to = "attribute", values_to = "old")

    out <- dplyr::left_join(new_subvars, old_subvars, by = c("alias", "attribute"))
    out <- dplyr::filter(out, .data$new != .data$old)
    out$type <- "var_meta_change"
    tidyr::nest(out, details = -c(.data$type, .data$alias))
}

find_cat_meta_changes <- function(old, new) {
    new_var_cats <- dplyr::filter(new, !.data$new_var & .data$type == "categorical")
    new_var_cats <- dplyr::mutate(new_var_cats, alias = ifelse(is.na(.data$parent_alias), .data$alias, .data$parent_alias))
    new_var_cats <- dplyr::select(new_var_cats, "alias", new_categories = "categories")
    new_var_cats <- dplyr::distinct(new_var_cats, .data$alias, .keep_all = TRUE)

    old_var_cats <- dplyr::filter(old, !.data$new_var & .data$type == "categorical")
    old_var_cats <- dplyr::mutate(old_var_cats, alias = ifelse(is.na(.data$parent_alias), .data$alias, .data$parent_alias))
    old_var_cats <- dplyr::select(old_var_cats, "alias", old_categories = "categories")
    old_var_cats <- dplyr::distinct(old_var_cats, .data$alias, .keep_all = TRUE)

    cats <- dplyr::full_join(new_var_cats, old_var_cats, by = "alias")

    # name/missing/value/date/selected changes
    cat_meta_change <-  dplyr::transmute(
        cats,
        alias = .data$alias,
        changes = purrr::map2(
            .data$old_categories,
            .data$new_categories,
            function (old_cats, new_cats) {
                new_cat_names <- dplyr::select(new_cats, "code", "name")
                new_cat_names <- dplyr::distinct(new_cat_names)

                old_cats <- dplyr::select(old_cats, -"orig_code")
                old_cats <- dplyr::mutate(old_cats, dplyr::across(-.data$code, as.list))
                old_cats <- tidyr::pivot_longer(old_cats, -.data$code, names_to = "attribute", values_to = "old")

                new_cats <- dplyr::select(new_cats, -"orig_code")
                new_cats <- dplyr::mutate(new_cats, dplyr::across(-.data$code, as.list))
                new_cats <- tidyr::pivot_longer(new_cats, -.data$code, names_to = "attribute", values_to = "new")

                cats <- dplyr::full_join(old_cats, new_cats, by = c("code", "attribute"))

                changes <- dplyr::filter(cats, purrr::map2_lgl(
                    .data$new,
                    .data$old,
                    ~.x != .y | (is.na(.x) & !is.na(.y)) | (!is.na(.x) & is.na(.y))
                ))
                if (nrow(changes) == 0) return(dplyr::tibble(type = character(0), details = list()))
                changes <- dplyr::left_join(changes, new_cat_names, by = "code")
                changes <- dplyr::mutate(changes, name = ifelse(.data$attribute == "name", unlist(.data$old), .data$name))
                changes <- dplyr::group_by(changes, .data$attribute)
                changes <- dplyr::summarize(
                    changes,
                    details = list(data.frame(name = .data$name, old = unlist(.data$old), new = unlist(.data$new)))
                )

                changes <- dplyr::arrange(changes, .data$attribute != "name")

                dplyr::tibble(
                    type = "cat_change",
                    details = list(changes)
                )

            })
    )
    cat_meta_change <- dplyr::filter(cat_meta_change, purrr::map_dbl(.data$changes, nrow) > 0)
    if (nrow(cat_meta_change) > 0) {
        cat_meta_change <- tidyr::unnest(cat_meta_change, .data$changes)
    } else {
        cat_meta_change <- NULL
    }

    # order changes
    cat_order <- dplyr::transmute(
        cats,
        type = "cat_reorder",
        alias = .data$alias,
        details = purrr::map2(
            .data$old_categories,
            .data$new_categories,
            function (old_cats, new_cats) {
                if (all(old_cats$code == new_cats$code)) return(NULL)
                dplyr::tibble(order = new_cats$name)
            })
    )
    cat_order <- dplyr::filter(cat_order, lengths(.data$details) > 0)

    bind_rows(cat_meta_change, cat_order)
}

validate_creations_only_array <- function(new) {
    non_array_new <- dplyr::filter(
        new,
        .data$new_var &
            is.na(.data$parent_alias) &
            !type %in% c("categorical_array", "multiple_response", "numeric_array")
    )
    if (nrow(non_array_new)) {
        stop(paste0(
            "Can only make new array variables, but found: ",
            paste0("'", non_array_new$alias, "'", collapse = ", ")
        ))
    }
}

find_array_creations <- function(old, new) {
    new_array_aliases <- dplyr::filter(
        new,
        .data$new_var & !is.na(.data$parent_alias)
    )
    new_array_aliases <- unique(new_array_aliases$parent_alias)

    new_var_info <- purrr::map(
        new_array_aliases,
        ~dplyr::filter(new, .data$alias == . | .data$parent_alias == .)
    )
    old_var_info <- purrr::map(
        new_var_info,
        ~dplyr::filter(old, .data$alias %in% .$alias[!is.na(.$parent_alias)])
    )

    purrr::map2_dfr(new_var_info, old_var_info, make_array_var)
}

make_array_var <- function(new_var, old_var) {
    parent_var <- dplyr::filter(new_var, is.na(.data$parent_alias))
    parent_alias <- parent_var$alias
    new_subvars <- dplyr::filter(new_var, !is.na(.data$parent_alias))

    missing_subvars <- setdiff(new_subvars$alias, old_var$alias)
    if (length(missing_subvars) > 0) {
        stop(paste0(
            "Could not find subvariables for ", parent_alias, " in original dataset: ",
            paste0("'", missing_subvar, "'", collapse = ", ")
        ), call. = FALSE)
    }

    new_cats <- dplyr::select(new_subvars, "alias", "categories")
    new_cats <- tidyr::unnest(new_cats, .data$categories)

    recode_attempts <- dplyr::filter(new_cats, .data$orig_code != .data$code)
    if (nrow(recode_attempts) > 0) {
        stop(paste0(
            "Cannot change category code when making an array ", parent_alias, ": ",
            paste0(recode_attempts$orig_code, "->", recode_attempts$code, collapse = ", ")
        ), call. = FALSE)
    }

    repeated_codes <- dplyr::group_by(new_cats, .data$alias, .data$code)
    repeated_codes <- dplyr::summarize(repeated_codes, n = n())
    repeated_codes <- dplyr::filter(repeated_codes, .data$n > 1)
    if (nrow(repeated_codes) > 0) {
        stop(paste0(
            "Found duplicate codes when making array ", parent_alias, ": ",
            paste0(repeated_codes$alias, "-", repeated_codes$code, collapse = ", ")
        ), call. = FALSE)
    }
    new_cats <- dplyr::select(new_cats, -"orig_code")

    is_mr <- any(new_cats$selected)
    selected_cats <- dplyr::filter(new_cats, !.data$missing)
    selected_cats <- dplyr::group_by(selected_cats, .data$alias, .data$selected)
    selected_cats <- dplyr::summarize(selected_cats, names = list(.data$name), codes = list(.data$code))
    selected_cats <- mutate(selected_cats, selected = ifelse(.data$selected, "sel", "not"))
    selected_cats <- tidyr::pivot_wider(
        selected_cats,
        .data$alias,
        names_from = .data$selected,
        values_from = c(.data$names, .data$codes)
    )

    new_cats <- dplyr::mutate(new_cats, dplyr::across(-c("alias", "code"), as.list))
    new_cats <- tidyr::pivot_longer(new_cats, -c("alias", "code"), names_to = "attribute", values_to = "new_value")

    old_cats <- dplyr::select(old_var, "alias", "categories")
    old_cats <- tidyr::unnest(old_cats, .data$categories)
    old_cats <- dplyr::select(old_cats, -"orig_code")
    old_cats <- dplyr::mutate(old_cats, dplyr::across(-c("alias", "code"), as.list))
    old_cats <- tidyr::pivot_longer(old_cats, -c("alias", "code"), names_to = "attribute", values_to = "old_value")

    cats_changed <- dplyr::full_join(new_cats, old_cats, by = c("alias", "code", "attribute"))
    cats_changed <- dplyr::filter(
        cats_changed,
        purrr::map2_lgl(.data$new_value, .data$old_value, ~.x != .y)
    )

    if (any(!cats_changed$attribute %in% c("selected"))) {
        stop(paste0(
            "Cannot change categories except for selected status: ", parent_alias
        ), call. = FALSE)
    }

    subvar_cat_equal <- all(purrr::map_lgl(new_subvars$categories, ~identical(., new_subvars$categories[[1]])))

    if (!is_mr) {
        dplyr::tibble(
            alias = parent_var$alias,
            type = "create_cat_array",
            details = list(dplyr::tibble(
                title = parent_var$title,
                description = parent_var$description,
                notes = parent_var$description,
                subvar_aliases = list(new_subvars$alias),
                subvar_labels = list(new_subvars$title)
            ))
        )
    } else {
        dplyr::tibble(
            alias = parent_var$alias,
            type = "create_mr_array",
            details = list(dplyr::tibble(
                title = parent_var$title,
                description = parent_var$description,
                notes = parent_var$description,
                subvar_aliases = list(new_subvars$alias),
                subvar_labels = list(new_subvars$title),
                selected = list(selected_cats)
            ))
        )
    }
}

find_folder_structure <- function(old, new) {
    new_dirs <- dplyr::filter(new, is.na(.data$parent_alias))
    new_dirs <- dplyr::group_by(new_dirs, .data$folder)
    new_dirs <- dplyr::summarize(new_dirs, new_aliases = list(.data$alias), .groups = "drop")

    old_dirs <- dplyr::filter(old, is.na(.data$parent_alias))
    old_dirs <- dplyr::group_by(old_dirs, .data$folder)
    old_dirs <- dplyr::summarize(old_dirs, old_aliases = list(.data$alias), .groups = "drop")

    dirs_compare <- dplyr::left_join(new_dirs, old_dirs, by = "folder")
    dirs_compare <- dplyr::filter(
        dirs_compare,
        !purrr::map2_lgl(.data$new_aliases, .data$old_aliases, identical)
    )

    dplyr::tibble(
        alias = NA_character_,
        type = "organize",
        details = purrr::map2(dirs_compare$folder, dirs_compare$new_aliases, ~dplyr::tibble(folder = .x, aliases = list(.y)))
    )
}
