#' Crunch ggplot theme
#'
#' Style ggplots according to Crunch style.
#'
#' @param base_size Base text size
#' @param base_family Base text family
#' @export
#' @importFrom ggplot2 element_line element_blank element_text element_rect
#' theme_minimal theme  rel unit
theme_crunch <- function(base_size = 12, base_family = "sans") {
    subtitle <- element_text(
                    hjust = 0,
                    face = "bold",
                    size = rel(1),
                    color = card_colors[1])
    (theme_minimal() +
            theme(
                line = element_line(colour = "black"),
                axis.title.y = element_blank(),
                axis.title.x =  element_text(
                    face = "bold",
                    size = rel(1),
                    color = card_colors[2]),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(
                    hjust = 0,
                    size = rel(1.5),
                    face = "bold",
                    color = card_colors[2]),
                plot.subtitle = subtitle,
                plot.margin = unit(c(1, 1, 1, 1), "lines"),
                legend.title = subtitle,
                strip.background = element_rect(fill = "light gray", linetype = 0))
    )
}

crunch_colors <- c(
    "#316395", "#cf3e3e", "#fcb73e", "#37ad6c", "#9537b5", "#17becf",
    "#e377c2", "#fdae6b", "#0099c6", "#ed5487", "#3366cc"
    )

card_colors <- c("#56A08E","#007F65")

#' @importFrom viridisLite viridis
#' @importFrom dplyr summarize pull
generate_colors <- function(var) {
    n_cols <- length(unique(var))
    if (n_cols > length(crunch_colors)) {
        return(c(crunch_colors, viridis(n_cols - length(crunch_colors))))
    } else {
        return(crunch_colors)
    }
}

#' Autoplot methods for Crunch Objects
#'
#' The Crunch autoplot methods generate `ggplots` that are tailored to various
#' Crunch objects. This allows you to visualize the object without bringing it
#' into memory. You can select between three families of plots, which will
#' attempt to accomodate the dimensionality of the plotted object. These plots
#' can be further extended and customized with other ggplot methods.
#'
#' @param x A Crunch variable or cube aggregation
#' @param ... additional plotting arguments
#' @param plot_type One of `"dot"`, `"tile"`, or `"bar"` which indicates the
#'   plot family you would like to use. Higher dimensional plots add color
#'   coding or facets depending on the dimensionality of the data.
#' @param measure The measure you wish to plot. This will usually be `"count"`,
#'   the default but can also be `".unweighted_counts"` or any other measure
#'   stored in the cube. If omitted, autoplot will select the first measure
#'   appearing in the data.
#' @return A `ggplot` object.
#' @name autoplot
#' @importFrom ggplot2 aes autoplot geom_histogram ggplot labs
#' @importFrom crunch description name
#' @importFrom tibble tibble
#' @export
autoplot.DatetimeVariable <- function(x, ...) {
    plot_df <- tibble(!!sym(name(x)) := as.Date(as.vector(x)))

    ggplot(plot_df, aes(x = !!sym(name(x)))) +
        geom_histogram(fill = card_colors[2]) +
        theme_crunch() +
        labs(title = name(x),
            subtitle = description(x))
}

#' @rdname autoplot
#' @importFrom ggplot2 aes autoplot geom_histogram ggplot labs
#' @importFrom crunch description name
#' @importFrom rlang !! sym :=
#' @importFrom tibble tibble
#' @export
autoplot.NumericVariable <- function(x, ...) {
    # TODO revisit when cut is implemented in zz9
    # https://www.pivotaltracker.com/n/projects/931610/stories/155299834
    v <- as.vector(x)
    plot_df <- tibble(!!sym(name(x)) := v)
    binwidth <- round((max(plot_df) - min(v)) / 5, 0)
    ggplot(plot_df, aes(x = !!sym(name(x)))) +
        geom_histogram(binwidth = binwidth, fill = card_colors[1]) +
        theme_crunch() +
        labs(title = name(x),
            subtitle = description(x))
}

#' @rdname autoplot
#' @export
autoplot.CategoricalVariable <- function(x, ...) plotCategorical(x, ...)

#' @rdname autoplot
#' @export
autoplot.CategoricalArrayVariable <- function(x, ...) plotCategorical(x, ...)

#' @rdname autoplot
#' @export
autoplot.MultipleResponseVariable <- function(x, ...) plotCategorical(x, ...)

#' @importFrom crunch alias datasetReference loadDataset
#' @importFrom ggplot2 autoplot
plotCategorical <- function(x, ...) {
    ds <- loadDataset(datasetReference(x))
    autoplot(crtabs(paste0("~", alias(x)), ds), ...)
}

plot_fun_lookup <- function(plot_dim, plot_type) {
    plot_fun <- paste0("crunch_", plot_dim, "d_", plot_type, "_plot")
    return(get(plot_fun))
}

#' @rdname autoplot
#' @export
autoplot.CrunchCube <- function(x,
    ...) {
    plot_tbl <- as_cr_tibble(x)
    autoplot(plot_tbl, ...)
}

#' @importFrom ggplot2 scale_x_continuous scale_fill_viridis_c scale_y_continuous
#' @rdname autoplot
#' @export
autoplot.CrunchCubeCalculation <- function(x,
                                           plot_type = "dot",
                                           ...) {
    plot_tbl <- as_cr_tibble(x)
    out <- autoplot(plot_tbl, plot_type, ...)
    if (attr(x, "type") == "proportion") {
        if (plot_type == "dot") {
            out <- out +
                scale_x_continuous(labels = scales::percent)
        }
        if (plot_type == "tile") {
            out <- out +
                scale_fill_viridis_c(labels = scales::percent)
        }
        if (plot_type == "bar") {
            out <- out +
                scale_y_continuous(labels = scales::percent)
        }
    }
    return(out)
}

#' @rdname autoplot
#' @importFrom rlang !! !!! .data sym syms
#' @importFrom purrr map map_chr
#' @importFrom dplyr mutate filter pull
#' @importFrom ggplot2 ggtitle
#' @export
autoplot.tbl_crunch_cube <- function(x,
                                plot_type = c("dot", "tile", "bar"),
                                measure) {
    plot_type <- match.arg(plot_type)
    display_names <- cube_attribute(x, "name")[is_dimension(x)]

    if (missing(measure)) {
        measure <- names(x)[dim_types(x) == "measure"][1]
    }

    if (length(measure) > 1) {
        # TODO think about how plots can support more than one measure.
        # for instance measures could be the grouping variable for 2d dot plots
        stop("Autoplot can only support one measure.", .call = FALSE)
    }
    measure <- sym(measure)

    # Remove missing values based on the useNA value for the cube.
    # TODO handle useNA = "ifany"
    plot_tbl <- as_tibble(x)
    if (attr(x, "useNA") == "no" && "is_missing" %in% names(x)) {
        plot_tbl <- plot_tbl[!plot_tbl$is_missing, ]
    }

    # Select the dimension columns from the table, this is necessary because the
    # names in the tibble are unique, while the cube dimnames are not.
    dim_names <- names(plot_tbl)[is_dimension(x)]

    # Only include rows where the MR selection dimensions are TRUE
    mr_selection_vars <- dim_names[dim_types(x) == "mr_selections" & is_dimension(x)]
    if (length(mr_selection_vars)) {
        plot_tbl <- plot_tbl %>%
            filter(!!!syms(mr_selection_vars))
    }
    #drop the MR selection dimensions for plotting
    dims <- syms(setdiff(dim_names, mr_selection_vars))

    # If the first two dimensions are CA dimensions, flip them. This is
    # because scales are usually on the second CA dimension.
    types <- dim_types(x)
    if (length(dims) != 1 && plot_type != "tile") {
        if (types[1] == "ca_items" && types[2] == "ca_categories") {
            dims[c(2, 1)] <- dims[(c(1, 2))]
        }
    }

    sub_text <- cube_attribute(x, "description")[1]
    if (is.na(sub_text)) {
        sub_text <- ""
    }

    plot_fun <- plot_fun_lookup(min(2, length(dims)), plot_type)

    out <- plot_fun(plot_tbl, dims, measure, display_names) +
        theme_crunch() +
        labs(title = paste0(unique(display_names), collapse = " + "),
            subtitle = sub_text)

    # Prevent duplicatation of legend name for categorical array
    if (types[2] == "ca_categories"){
        out <- out + theme(legend.title=element_blank())
    }

    if (plot_type == "tile") {
        # This is here instead of in the 2d_tile plot function because theme_crunch
        # overrides the axis.text property
        out <- out +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

    # If there are more than two dimensions, add facets for the remaing dimensions
    # TODO consider adding a max number of dimensions.
    if (length(dims) > 2) {
        out <- add_facets(out, dims[3:length(dims)])
    }
    return(out)
}

#' @importFrom dplyr arrange desc mutate %>%
#' @importFrom stats reorder
.crunch_1d_tibble <- function(tibble, dims, measure, display_names) {
    tibble %>%
        arrange(desc(!!measure)) %>%
        mutate(!!dims[[1]] := reorder(!!dims[[1]], !!measure))
}

#' @importFrom ggplot2 geom_point ggplot
crunch_1d_dot_plot <- function(tibble, dims, measure, display_names){
    .crunch_1d_tibble(tibble, dims, measure) %>%
        ggplot(aes(y = !!dims[[1]], x = !!measure)) +
        geom_point(color = card_colors[2], size = 2)
}

#' @importFrom ggplot2 coord_flip geom_bar ggplot
crunch_1d_bar_plot <- function(tibble, dims, measure, display_names){
    .crunch_1d_tibble(tibble, dims, measure) %>%
        ggplot(aes(x = !!dims[[1]], y = !!measure)) +
        geom_bar(stat = "identity", fill = card_colors[2]) +
        coord_flip()
}

#' @importFrom ggplot2 coord_flip geom_raster xlab scale_fill_viridis_c
crunch_1d_tile_plot <- function(tibble, dims, measure, display_names) {
    .crunch_1d_tibble(tibble, dims, measure) %>%
        ggplot(aes(y = !!dims[[1]], x = deparse(measure), fill = !!measure)) +
        xlab("") +
        geom_raster() +
        scale_fill_viridis_c()
}

#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes geom_point ggplot scale_fill_viridis_c geom_raster
crunch_2d_tile_plot <- function(tibble, dims, measure, display_names) {
    tibble %>%
        mutate(!!measure := ifelse(!!measure == 0, NA, !!measure)) %>%
        ggplot(aes(x = !!dims[[1]], y = !!dims[[2]], fill = !!measure)) +
        geom_raster() +
        scale_fill_viridis_c()
}

#' @importFrom ggplot2 ggplot geom_point labs scale_color_manual
crunch_2d_dot_plot <- function(tibble, dims, measure, display_names) {
    cols <- generate_colors(tibble[[as.character(dims[[2]])]])
    tibble %>%
        .crunch_2d_tibble(dims, measure) %>%
        ggplot(aes(
            x = !!measure,
            y = !!dims[[1]],
            group = !!dims[[2]],
            color = !!dims[[2]])) +
        geom_point(size = 2) +
        scale_color_manual(values = cols) +
        labs(color = display_names[2])
}

#' @importFrom ggplot2 aes coord_flip ggplot geom_bar labs scale_fill_manual
crunch_2d_bar_plot <- function(tibble, dims, measure, display_names) {
    cols <- generate_colors(tibble[[as.character(dims[[2]])]])
    tibble %>%
        .crunch_2d_tibble(dims, measure) %>%
        ggplot(aes(
            x = !!dims[[1]],
            y = !!measure,
            group = !!dims[[2]],
            fill = !!dims[[2]])) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = cols) +
        labs(fill = display_names[2])
}

#' @importFrom dplyr arrange group_by mutate pull summarize
#' @importFrom rlang .data
.crunch_2d_tibble <- function(tibble, dims, measure) {
    levs <- tibble %>%
        group_by(!!dims[[1]]) %>%
        summarize(order_var = sum(!!measure)) %>%
        arrange(.data$order_var) %>%
        pull(!!dims[[1]])

    out <- tibble %>%
        mutate(!!dims[[1]] := factor(!!dims[[1]], levels = levs))
    return(out)
}
#' @importFrom ggplot2 facet_wrap facet_grid vars
add_facets <- function(plot, facet_dims) {
    n_dims <- length(facet_dims)
    if (n_dims == 1) {
        plot +
            facet_wrap(vars(!!facet_dims[[1]]))
    } else {
        idx <- ceiling(n_dims / 2)
        plot +
            facet_grid(
                vars(!!!facet_dims[(idx + 1):n_dims]),
                vars(!!!facet_dims[1:idx])
            )
    }
}
