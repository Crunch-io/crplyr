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

    new_cats <- details$new_categories[[1]]
    new_cats <- dplyr::mutate(new_cats, order = dplyr::row_number())
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
                "{ifelse(!is.na(value), value, '')}{ifelse(missing, ' MISSING', '')}",
                .trim = FALSE
            )
        })
    cat_text <- paste(cat_text, collapse = "\n")

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
        "REORDER CATEGORIES {alias} ORDERED\n    {order};\n",
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
        glue::glue_collapse(name_lines, "\n"),
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
        glue::glue("    \"{name}\" = {new}", .trim = FALSE)
    })
    glue::glue(
        "SET DATES {alias} WITH\n",
        glue::glue_collapse(name_lines, "\n"),
        ";\n",
        .trim = FALSE
    )
}

cmd_gen_create_mr_array <- function(alias, details) {
    glue::glue("# create mr array {alias} - not implemented yet\n", .trim = FALSE)
}

cmd_gen_create_cat_array <- function(alias, details) {
    glue::glue("# create cat array {alias} - not implemented yet\n", .trim = FALSE)
}

cmd_gen_organize <- function(alias, details) {
    folder <- details$folder
    hidden_regex <- stringr::regex("^HIDDEN\\|", ignore_case = TRUE)
    secure_regex <- stringr::regex("^SECURE\\|", ignore_case = TRUE)
    if (folder == "|") {
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
        dir  <- stringr::str_replace(folder, "^\\|", "")
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
