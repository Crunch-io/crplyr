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
    save_excel_dictionary(dictionary_data, out_file, overwrite)
}

build_excel_dictionary_data <- function(ds) {
    with(
        crunch::temp.option(crunch.warn.hidden = FALSE), {
            # Variable + Type ---
            all_vars <- crunch::allVariables(ds)
            catalog_info <- dplyr::tibble(
                type = crunch::types(all_vars),
                alias = crunch::aliases(all_vars),
                orig_alias = crunch::aliases(all_vars),
                orig_child_alias = NA_character_,
                title = names(all_vars),
                description = crunch::descriptions(all_vars),
                notes = crunch::notes(all_vars)
            )
            # users of excel won't be able to tell the difference between "" and NA so conver to NA
            catalog_info <- dplyr::mutate(
                catalog_info,
                dplyr::across(dplyr::one_of(c("description", "notes")), ~ifelse(. == "", NA_character_, .))
            )

            # Categories ----
            vars_with_cats <- dplyr::filter(
                catalog_info,
                type %in% c("categorical", "categorical_array", "multiple_response")
            )
            vars_with_cats <- vars_with_cats[["alias"]]
            categories <- build_categories(ds, vars_with_cats)

            # Subvariables ----
            vars_with_subvars <- dplyr::filter(
                catalog_info,
                type %in% c("categorical_array", "multiple_response", "numeric_array")
            )
            vars_with_subvars <- vars_with_subvars[["alias"]]
            subvars <- build_subvars(ds, vars_with_subvars)

            # Directory Structure ----
            folder_info <- dplyr::bind_rows(
                dir_tree(cd(ds, "/")),
                dir_tree(hiddenFolder(ds), "Hidden|"),
                dir_tree(privateFolder(ds), "Private|"),
            )

            folder_info <- dplyr::mutate(folder_info, var_order = dplyr::row_number())

            # Combine ----
            out_columns <- c(
                "type", "folder", "orig_alias", "orig_child_alias", "alias", "title",
                "description", "notes", "subvars", "categories"
            )

            combined <- dplyr::full_join(folder_info, catalog_info, by = "orig_alias")
            combined <- dplyr::full_join(combined, subvars, by = "orig_alias")
            combined <- dplyr::full_join(combined, categories, by = "orig_alias")
            combined <- dplyr::arrange(combined, .data$var_order)
            combined <- dplyr::select(combined, dplyr::one_of(out_columns))
            combined
        })
}

build_categories <- function(ds, vars_with_cats) {
    if (length(vars_with_cats) == 0) {
        return(dplyr::tibble(alias = character(0), categories = list()))
    }

    categories <- purrr::map_dfr(
        vars_with_cats,
        function(alias) {
            cats <- categories(ds[[alias]])
            out <- dplyr::tibble(
                orig_alias = alias,
                child_alias = NA_character_,
                orig_code = ids(cats),
                code = ids(cats),
                name = names(cats),
                missing = is.na(cats),
                selected = is.selected(cats),
                value = values(cats),
                date = dates(cats)
            )
        }
    )
    # users of excel won't be able to tell the difference between "" and NA so conver to NA
    categories <- dplyr::mutate(
        categories,
        dplyr::across(dplyr::one_of(c("date")), ~ifelse(. == "", NA_character_, .))
    )
    categories <- dplyr::nest_by(categories, .data$orig_alias, .key = "categories")
    categories
}

build_subvars <- function(ds, vars_with_subvars) {
    if (length(vars_with_subvars) == 0) {
        return(dplyr::tibble(alias = character(0), subvars = list()))
    }

    subvars <- purrr::map_dfr(
        vars_with_subvars,
        function(alias) {
            subvars <- crunch::subvariables(ds[[alias]])
            dplyr::tibble(
                type = crunch::types(subvars),
                orig_alias = alias,
                orig_child_alias = crunch::aliases(subvars),
                child_alias = crunch::aliases(subvars),
                subvar_label = names(subvars)
            )
        }
    )
    subvars <- dplyr::nest_by(subvars, .data$orig_alias, .key = "subvars", .keep = TRUE)
    subvars
}

dir_tree <- function(folder, path = "|") {
    tree <- dir_tree_named(folder, path)
    dplyr::tibble(
        orig_alias = unname(tree),
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
    var_info <- dplyr::mutate(data, var_order = dplyr::row_number())

    subvars <- dplyr::filter(var_info, purrr::map_lgl(.data$subvars, ~!is.null(.) && nrow(.) > 0))
    subvars <- dplyr::select(
        subvars,
        c("var_order", "folder", "alias", "title", "description", "notes", "orig_alias", "subvars")
    )
    subvars <- tidyr::unnest(subvars, .data$subvars)
    subvars <- dplyr::group_by(subvars, .data$var_order)
    subvars <- dplyr::mutate(
        subvars,
        dplyr::across(c("folder", "title", "description", "notes"), ~ifelse(dplyr::row_number() == 1, ., NA))
    )
    subvars <- dplyr::mutate(subvars, sv_order = dplyr::row_number(), cat_order = -Inf)

    categories <- dplyr::filter(var_info, purrr::map_lgl(.data$categories, ~!is.null(.) && nrow(.) > 0))
    categories <- dplyr::select(categories, c("var_order", "orig_alias", "categories"))
    categories <- tidyr::unnest(categories, .data$categories)
    categories <- dplyr::mutate(categories, sv_order = Inf, cat_order = dplyr::row_number())

    var_info <- dplyr::filter(var_info, purrr::map_lgl(.data$subvars, ~is.null(.) || nrow(.) == 0))
    var_info <- dplyr::select(var_info, -c("subvars", "categories"))
    var_info <- dplyr::mutate(var_info, sv_order = -Inf, cat_order = -Inf)

    out <- dplyr::bind_rows(var_info, subvars, categories)
    out <- dplyr::arrange(out, .data$var_order, .data$sv_order, .data$cat_order)
    out <- dplyr::select(out, dplyr::one_of(c(
        "type", "folder", "orig_alias", "orig_child_alias", "alias", "child_alias", "title",
        "subvar_label", "description", "notes", "orig_code", "code", "name", "missing", "selected",
        "value", "date"
    )))

    out <- dplyr::mutate(out, dplyr::across(c("missing", "selected"), ~ifelse(., "X", NA)))
    out <- dplyr::mutate(out, dplyr::across(c("title", "description", "notes"), ~ifelse(. == "", NA, .)))

    wb <- openxlsx::createWorkbook(creator = "crunch")
    ws <- openxlsx::addWorksheet(wb, "Variables Editor")
    openxlsx::setColWidths(wb, ws, 1:17, c(16, 30, 10, 10, 10, 10, 25, 25, 25, 25, 7.5, 10, 25, 7.5, 7.5, 7.5, 7.5))
    openxlsx::setColWidths(wb, ws, 18, hidden = TRUE)
    openxlsx::mergeCells(wb, ws, 11:17, 1)
    openxlsx::writeData(wb, ws, "Categories", startCol = 11, startRow = 1)
    openxlsx::writeData(
        wb,
        ws,
        as.data.frame(matrix(nrow = 1, ncol = 17, c(
            "Type", "Folder", "Original Alias", "Original Child Alias", "Alias", "Child Alias", "Title",
            "Subvariable Label", "Description", "Notes", "Original Code", "Code", "Name", "Missing",
            "Selected", "Value", "Date"
        ))),
        startCol = 1,
        startRow = 2,
        colNames = FALSE
    )

    openxlsx::writeData(wb, ws, out, startRow = 3, colNames = FALSE)

    openxlsx::writeData(wb, ws, "Calc Alias", startCol = 17 + 1, startRow = 2)
    openxlsx::writeFormula(
        wb,
        ws,
        glue::glue(
            "=IF({MAIN_COL}{ROW}<>\"\",{MAIN_COL}{ROW},OFFSET({CALC_COL}{ROW},-1,0))",
            MAIN_COL = "E",
            CALC_COL = "R",
            ROW = 3:(nrow(out) + 202)
        ),
        startCol = 17 + 1,
        startRow = 3
    )
    header_style <- openxlsx::createStyle(textDecoration = "bold")
    header_ul_style <- openxlsx::createStyle(border = "bottom", halign = "center")
    dont_edit_style <- openxlsx::createStyle(fontColour = "#CCCCCC")
    beginning_of_var_style <- openxlsx::createStyle(border = "top", borderColour = "#888888")
    openxlsx::conditionalFormatting(
        wb,
        ws,
        1:17,
        3:(nrow(out) + 202),
        glue::glue("${CALC_COL}3<>OFFSET(${CALC_COL}3,-1,0)", CALC_COL = "R"),
        beginning_of_var_style
    )
    # work around openxlsx limitation that you can't stack conditional formatting on top of other
    # formattings, so munge the conditional formatting style
    wb$styles$dxfs[1] <- paste0(
        "<dxf><font><sz val=\"11\"/></font><border><top style=\"thin\">",
        "<color rgb=\"FF888888\"/></top></border></dxf>"
    )
    openxlsx::addStyle(wb, ws, header_ul_style, 1, 11:17, stack = TRUE)
    openxlsx::addStyle(wb, ws, header_style, 1, 1:18, stack = TRUE)
    openxlsx::addStyle(wb, ws, header_style, 2, 1:18, stack = TRUE)
    openxlsx::addStyle(wb, ws, dont_edit_style, 2:(nrow(out) + 202), 1, stack = TRUE)
    openxlsx::addStyle(wb, ws, dont_edit_style, 2:(nrow(out) + 202), 3, stack = TRUE)
    openxlsx::addStyle(wb, ws, dont_edit_style, 2:(nrow(out) + 202), 4, stack = TRUE)
    openxlsx::addStyle(wb, ws, dont_edit_style, 2:(nrow(out) + 202), 11, stack = TRUE)
    openxlsx::addStyle(wb, ws, dont_edit_style, 2:(nrow(out) + 202), 11, stack = TRUE)
    openxlsx::addStyle(wb, ws, dont_edit_style, 2:(nrow(out) + 202), 18, stack = TRUE)
    openxlsx::freezePane(wb, ws, firstActiveRow = 3)
    openxlsx::saveWorkbook(wb, out_file, overwrite = overwrite)
}
