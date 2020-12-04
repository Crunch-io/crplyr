#' Generate a Crunch Automation Script from an excel variable editor
#'
#' Details forthcoming. Experimental.
#'
#' @param dataset A `CrunchDataset` the variable editor describes
#' @param excel_file Filepath to an .xlsx file, generally an excel document
#' created by [`create_excel_dictionary()`] that you've modified
#' @param out_file A filepath to save the Crunch Automation Script to (`NULL`,
#' the default, places the file in your temporary directory)
#' @param submit Logical, indicating whether to submit the generated script to be run
#' (defaults to `TRUE`)
#'
#' @return
#' @export
apply_excel_dictionary <- function(dataset, excel_file, out_file = NULL, submit = TRUE) {
    altered_schema <- read_excel_editor(excel_file)
    current_schema <- build_excel_dictionary_data(dataset)
    commands <- find_changes(current_schema, altered_schema)
    command_text <- generate_code(commands, dataset, excel_file)

    if (is.null(out_file)) {
        out_file <- tempfile("ca_script", fileext = ".txt")
    }

    writeLines(command_text, out_file)
    if (submit) {
        crunch::runCrunchAutomation(ds, out_file, is_file = TRUE)
    } else {
        cat(paste0("Crunch Automation script written to: ", out_file, "\n"))
    }
    invisible(out_file)
}

generate_code <- function(cmds, dataset, file) {
    glue::glue(
        "# --- Crunch Automation code for dataset: '{name(dataset)}'\n",
        "# --- {self(dataset)}\n",
        "# --- Generated from: '{basename(file)}'\n",
        "\n",
        "{generate_code_block(cmds$schema_commands, 'Schema changes')}",
        "{generate_code_block(cmds$alter_commands, 'Variable alterations')}",
        "{generate_code_block(cmds$create_commands, 'Variable creations')}",
        "{generate_code_block(cmds$organize_commands, 'Organization')}"
    )
}

generate_code_block <- function(cmds, comment) {
    if (nrow(cmds) == 0) return("")
    command_text <- generate_commands(cmds)
    glue::glue(
        "# --- {comment} ---\n{command_text}\n"
    )
}

generate_commands <- function(cmds) {
    paste(purrr::pmap_chr(cmds, generate_command), collapse = "")
}

generate_command <- function(alias, type, details) {
    if (!type %in% names(cmd_generators)) {
        stop(paste0("Could not find code generator for '", type, "'"), call. = FALSE)
    }
    cmd_generators[[type]](alias, details)
}

cmd_gen_recode <- function(alias, details) {
    old_names <- details$old_categories[[1]]
    old_names <- dplyr::select(old_names, "orig_code", "orig_name" = "name")
    old_names <- tidyr::unnest(old_names, .data$orig_code)

    new_cats <- details$new_categories[[1]]
    new_cats <- dplyr::mutate(new_cats, order = dplyr::row_number())
    new_cats <- tidyr::unnest(new_cats, c(.data$orig_code))
    new_cats <- dplyr::left_join(new_cats, old_names, by = "orig_code")

    new_cats <- dplyr::group_by(new_cats, .data$code)
    new_cats <- dplyr::summarize(
        new_cats,
        orig_code = paste0(.data$orig_code, collapse = ", "),
        orig_name = paste0('"', .data$orig_name, '"', collapse = ", "),
        dplyr::across(-dplyr::one_of("orig_code"), ~.[1]),
        .groups = "drop"
    )
    new_cats <- dplyr::arrange(new_cats, .data$order)
    new_cats <- dplyr::select(new_cats, -"order", -"orig_code")

    cat_text <- purrr::pmap_chr(
        new_cats,
        function(code, orig_name, name, missing, selected, value, date) {
            glue::glue(
                "        {orig_name} INTO \"{name}\" CODE {code}",
                "{ifelse(missing, ' MISSING', '')}",
                .trim = FALSE
            )
        })
    cat_text <- paste(cat_text, collapse = ", \n")

    glue::glue(
        "REPLACE CATEGORICAL RECODE {alias}\n",
        "    MAPPING\n",
        "{cat_text};\n\n",
        .trim = FALSE
    )

}

cmd_gen_rename <- function(alias, details) {
    glue::glue(
        "RENAME {details$from} TO {details$to};\n\n", .trim = FALSE
    )
}

cmd_gen_var_meta_change <- function(alias, details) {
    out <- purrr::pmap_chr(details, function(attribute, new, old) {
        glue::glue("CHANGE {toupper(attribute)} IN {alias} WITH \"{new}\";", .trim = FALSE)
    })
    glue::glue(glue::glue_collapse(out, "\n"), "\n\n", .trim = FALSE)
}

cmd_gen_cat_reorder <- function(alias, details) {
    order <- paste0("\"", details$order, "\"", collapse = ", ")
    glue::glue(
        "REORDER CATEGORIES {alias} ORDERED\n    {order};\n\n",
        .trim = FALSE
    )
}

cmd_gen_cat_change <- function(alias, details) {
    out <- purrr::pmap_chr(details, function(attribute, details) {
        switch(
            attribute,
            "name" = cmd_gen_cat_change_name(alias, details),
            "value" = cmd_gen_cat_change_value(alias, details),
            "missing" = cmd_gen_cat_change_missing(alias, details),
            "date" = cmd_gen_cat_change_date(alias, details),
            stop(glue::glue("Do not know how to change category metadate type '{attribute}'"))
        )
    })

    glue::glue("{glue::glue_collapse(out, '')}\n", .trim = FALSE)
}

cmd_gen_cat_change_name <- function(alias, details) {
    name_lines <- purrr::pmap_chr(details, function(name, old, new) {
        glue::glue("    \"{name}\" AS \"{new}\"", .trim = FALSE)
    })
    glue::glue(
        "LABEL CATEGORIES {alias} WITH\n",
        glue::glue_collapse(name_lines, "\n"),
        ";\n",
        .trim = FALSE
    )
}

cmd_gen_cat_change_value <- function(alias, details) {
    name_lines <- purrr::pmap_chr(details, function(name, old, new) {
        new[is.na(new)] <- "NULL"
        glue::glue("    \"{name}\" = {new}", .trim = FALSE)
    })
    glue::glue(
        "SET NUMERIC VALUE {alias} WITH\n",
        glue::glue_collapse(name_lines, ", \n"),
        ";\n",
        .trim = FALSE
    )
}

cmd_gen_cat_change_missing <- function(alias, details) {
    missing <- dplyr::filter(details, .data$new)
    nonmissing <- dplyr::filter(details, !.data$new)

    out <- ""
    if (nrow(missing) > 0) {
        names <- paste0("\"", missing$name, "\"", collapse = ", ")
        out <- glue::glue("{out}SET MISSING {names} IN {alias};\n", .trim = FALSE)
    }
    if (nrow(nonmissing) > 0) {
        names <- paste0("\"", nonmissing$name, "\"", collapse = ", ")
        out <- glue::glue("{out}SET NOT MISSING {names} IN {alias};\n", .trim = FALSE)
    }
    out
}

cmd_gen_cat_change_date <- function(alias, details) {
    name_lines <- purrr::pmap_chr(details, function(name, old, new) {
        new[is.na(new)] <- "NULL"
        glue::glue("    \"{name}\" = \"{new}\"", .trim = FALSE)
    })
    glue::glue(
        "SET DATES {alias} WITH\n",
        glue::glue_collapse(name_lines, "\n"),
        ";\n",
        .trim = FALSE
    )
}

cmd_gen_create_mr_array <- function(alias, details) {
    sv_labels <- paste0("\"", details$subvar_labels[[1]], "\"", collapse = ", ")
    title <- glue::glue("TITLE \"{details$title}\"", .trim = FALSE)
    description <- if (is.na(details$description)) "" else glue::glue("\nDESCRIPTION \"{details$description}\"", .trim = FALSE)
    notes <- if (is.na(details$notes)) "" else glue::glue("\nNOTES \"{details$notes}\"", .trim = FALSE)

    selections <- details$selected[[1]]
    simple_dichotomy <- dplyr::summarize(
        dplyr::ungroup(selections),
        dplyr::across(-"alias", ~purrr::map_lgl(., function(x) identical(x, .[[1]])))
    )
    simple_dichotomy <- all(unlist(simple_dichotomy))

    if (simple_dichotomy) {
        sv_aliases <- paste0(details$subvar_aliases[[1]], collapse = ", ")
        selected <- paste0("\"", selections$names_sel[[1]], "\"", collapse = ", ")
        glue::glue(
            "CREATE MULTIPLE DICHOTOMY FROM\n",
            "    {sv_aliases}\n",
            "    LABELS {sv_labels}\n",
            "    SELECTED {selected}\n",
            "AS {alias}\n",
            "{title}{description}{notes};\n\n",
            .trim = FALSE
        )
    } else {
        sv_recodes <- dplyr::transmute(
            selections,
            alias = .data$alias,
            dplyr::across(c("names_sel", "names_not"), ~paste0("\"", ., "\"", collapse = ", "))
        )
        sv_recodes$label <- ifelse(
            is.na(details$subvar_labels[[1]]),
            paste0(" LABEL ", details$subvar_labels[[1]]),
            ""
        )
        sv_recodes <- purrr::pmap_chr(
            sv_recodes,
            function(alias, label, names_sel, names_not) {
                glue::glue("{alias} (SELECTED {names_sel} NOT SELECTED {names_not}{label})")
            }
        )
        sv_recodes <- paste("    ", sv_recodes, collapse = ",\n")
        glue::glue(
            "CREATE MULTIPLE DICHOTOMY WITH RECODE\n",
            "{sv_recodes}\n",
            "AS {alias}\n",
            "{title}{description}{notes};\n\n",
            .trim = FALSE
        )
    }
}

cmd_gen_create_cat_array <- function(alias, details) {
    sv_aliases <- paste0(details$subvar_aliases[[1]], collapse = ", ")
    sv_labels <- paste0("\"", details$subvar_labels[[1]], "\"", collapse = ", ")
    title <- glue::glue("TITLE \"{details$title}\"", .trim = FALSE)
    description <- if (is.na(details$description)) "" else glue::glue("\nDESCRIPTION \"{details$description}\"", .trim = FALSE)
    notes <- if (is.na(details$notes)) "" else glue::glue("\nNOTES \"{details$notes}\"", .trim = FALSE)
    glue::glue(
        "CREATE CATEGORICAL ARRAY \n",
        "    {sv_aliases}\n",
        "    LABELS {sv_labels}\n",
        "AS {alias}\n",
        "{title}{description}{notes};\n\n",
        .trim = FALSE
    )

}

cmd_gen_organize <- function(alias, details) {
    folder <- details$folder
    hidden_regex <- stringr::regex("^HIDDEN\\|", ignore_case = TRUE)
    secure_regex <- stringr::regex("^SECURE\\|", ignore_case = TRUE)
    if (is.na(folder) || folder == "|") {
        dir <- "ROOT"
    } else if (stringr::str_detect(folder, hidden_regex)) {
        path <- stringr::str_replace(folder, hidden_regex, "")
        if (path != "") path <- paste0(" \"", path, "\"")
        dir <- glue::glue("HIDDEN{path}")
    } else if (stringr::str_detect(folder, secure_regex)) {
        path <- stringr::str_replace(folder, secure_regex, "")
        if (path != "") path <- paste0(" \"", path, "\"")
        dir <- glue::glue("SECURE{path}")
    } else {
        dir  <- paste0("\"", stringr::str_replace(folder, "^\\|", ""), "\"")
    }

    vars <- paste0(details$aliases[[1]], collapse = ", ")

    glue::glue("ORGANIZE {vars}\n    INTO {dir};\n\n", .trim = FALSE)
}

cmd_generators <- list(
    rename = cmd_gen_rename,
    recode = cmd_gen_recode,
    var_meta_change = cmd_gen_var_meta_change,
    cat_reorder = cmd_gen_cat_reorder,
    cat_change = cmd_gen_cat_change,
    create_mr_array = cmd_gen_create_mr_array,
    create_cat_array = cmd_gen_create_cat_array,
    organize = cmd_gen_organize
)
