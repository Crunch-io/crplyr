#' Save an Excel document that describes the variables in a Crunch Dataset (EXPERIMENTAL)
#'
#' Create an MS Excel document that describes the variables and their metadata
#' (type, alias, title, description, notes, subvariables and categories). This
#' function is under active development and so its structure may change.
#'
#' @param ds A `CrunchDataset`
#' @param out_file File path to save the excel `.xlsx` document
#' @param overwrite Whether to overwrite the file (default `FALSE`)
#'
#' @export
create_excel_dictionary <- function(ds, out_file = NULL, overwrite = FALSE) {
    dictionary_data <- build_excel_dictionary_data(ds)
    if (is.null(out_file)) return(dictionary_data)
    save_excel_dictionary(dictionary_data, out_file)
}

build_excel_dictionary_data <- function(ds) {
    with(
        crunch::temp.option(crunch.warn.hidden = FALSE), {
            # Variable + Type ---
            all_vars <- crunch::allVariables(ds)
            catalog_info <- dplyr::tibble(
                type = crunch::types(all_vars),
                alias = crunch::aliases(all_vars),
                title = names(all_vars),
                description = crunch::descriptions(all_vars),
                notes = crunch::notes(all_vars)
            )


            # Categories ----
            vars_with_cats <- dplyr::filter(
                catalog_info,
                type %in% c("categorical", "categorical_array", "multiple_response")
            )
            vars_with_cats <- vars_with_cats[["alias"]]

            categories <- purrr::map_dfr(
                vars_with_cats,
                function(alias) {
                    cats <- categories(ds[[alias]])
                    out <- dplyr::tibble(
                        alias = alias,
                        code = ids(cats),
                        name = names(cats),
                        missing = is.na(cats),
                        selected = is.selected(cats),
                        value = values(cats),
                        date = dates(cats)
                    )
                }
            )
            categories <- dplyr::mutate(categories, category_order = dplyr::row_number())

            # Subvariables ----
            vars_with_subvars <- dplyr::filter(
                catalog_info,
                type %in% c("categorical_array", "multiple_response")
            )
            vars_with_subvars <- vars_with_subvars[["alias"]]

            subvars <- purrr::map_dfr(
                vars_with_subvars,
                function(alias) {
                    subvars <- crunch::subvariables(ds[[alias]])
                    dplyr::tibble(
                        alias = alias,
                        type = crunch::types(subvars),
                        child_alias = crunch::aliases(subvars),
                        title = names(subvars),
                        description = crunch::descriptions(subvars),
                        notes = crunch::notes(subvars)
                    )
                }
            )
            subvars <- dplyr::mutate(subvars, subvar_order = dplyr::row_number())

            # Directory Structure ----
            folder_info <- dplyr::bind_rows(
                dir_tree(cd(ds, "/")),
                dir_tree(hiddenFolder(ds), "Hidden|"),
                dir_tree(privateFolder(ds), "Private|"),
            )
            folder_info <- dplyr::mutate(folder_info, var_order = dplyr::row_number())


            # Combine ----
            out_columns <- c(
                "type", "folder", "orig_alias", "orig_child_alias", "alias", "child_alias", "title",
                "description", "notes", "orig_code", "code", "name", "missing", "selected", "value",
                "date"
            )
            categories <- dplyr::left_join(categories, folder_info[c("alias", "var_order")], by = "alias")
            subvars <- dplyr::left_join(subvars, folder_info[c("alias", "var_order")], by = "alias")

            combined <- dplyr::full_join(folder_info, catalog_info, by = "alias")
            combined <- dplyr::mutate(combined, subvar_order = -Inf)
            combined <- dplyr::bind_rows(combined, subvars)
            combined <- dplyr::mutate(combined, category_order = -Inf)
            combined <- dplyr::bind_rows(combined, categories)
            combined <- dplyr::mutate(
                combined,
                orig_alias = alias,
                orig_child_alias = child_alias,
                alias = ifelse(is.na(child_alias) & is.na(code), alias, NA),
                orig_code = code
            )
            combined <- dplyr::arrange(combined, var_order, category_order, subvar_order)
            combined <- dplyr::select(combined, dplyr::one_of(out_columns))

            combined
        })
}

dir_tree <- function(folder, path = "|") {
    tree <- dir_tree_named(folder, path)
    dplyr::tibble(
        alias = unname(tree),
        folder = names(tree)
    )
}

dir_tree_named <- function(folder, path = "|") {
    out <- lapply(
        seq_along(folder),
        function(i) {
            if (crunch::is.variable(folder[[i]])) {
                stats::setNames(crunch::alias(folder[[i]]), path)
            } else {
                dir_tree_named(folder[[i]], paste0(path, crunch::name(folder[[i]]), "|"))
            }
        }
    )
    unlist(out)
}

save_excel_dictionary <- function(data, out_file, overwrite = FALSE) {
    data <- dplyr::mutate(data, dplyr::across(c("missing", "selected"), ~ifelse(., "X", NA)))
    data <- dplyr::mutate(data, dplyr::across(c("title", "description", "notes"), ~ifelse(. == "", NA, .)))

    wb <- openxlsx::createWorkbook(creator = "crunch")
    ws <- openxlsx::addWorksheet(wb, "Variables Editor")
    openxlsx::setColWidths(wb, ws, 1:16, c(16, 30, 10, 10, 3, 30, 25, 25, 25, 7.5, 10, 25, 7.5, 7.5, 7.5, 7.5))

    openxlsx::mergeCells(wb, ws, 10:16, 1)
    openxlsx::writeData(wb, ws, "Categories", startCol = 10, startRow = 1)
    openxlsx::writeData(
        wb,
        ws,
        as.data.frame(matrix(nrow = 1, ncol = 16, c(
            "Type", "Folder", "Original Alias", "Original Child Alias", "Alias", NA, "Title", "Description", "Notes",
            "Original Code", "Code", "Name", "Missing", "Selected", "Value", "Date"
        ))),
        startCol = 1,
        startRow = 2,
        colNames = FALSE
    )

    header_style <- openxlsx::createStyle(textDecoration = "bold")
    header_ul_style <- openxlsx::createStyle( border = "bottom", halign = "center")
    dont_edit_style <- openxlsx::createStyle(fontColour = "#CCCCCC")
    openxlsx::addStyle(wb, ws, header_ul_style, 1, 10:16)
    openxlsx::addStyle(wb, ws, header_style, 1, 1:16, stack = TRUE)
    openxlsx::addStyle(wb, ws, header_style, 2, 1:16, stack = TRUE)
    openxlsx::addStyle(wb, ws, dont_edit_style, 2:(nrow(data) + 2), 3, stack = TRUE)
    openxlsx::addStyle(wb, ws, dont_edit_style, 2:(nrow(data) + 2), 4, stack = TRUE)
    openxlsx::addStyle(wb, ws, dont_edit_style, 2:(nrow(data) + 2), 10, stack = TRUE)
    openxlsx::freezePane(wb, ws, firstActiveRow = 3)
    openxlsx::writeData(wb, ws, data, startRow = 3, colNames = FALSE)
    openxlsx::saveWorkbook(wb, out_file, overwrite = overwrite)
}
