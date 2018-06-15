
#' Crunch ggplot theme
#' 
#' Style ggplots according to Crunch style.
#'
#' @param base_size Base text size
#' @param base_family Base text family
#'
#' @export
#' 
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
                axis.title = element_blank(),
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


#' @rdname autoplot
#' @importFrom ggplot2 aes autoplot geom_histogram ggplot labs
#' @importFrom crunch description name
#' @export
autoplot.DatetimeVariable <- function(x, ...) {
    v <- as.vector(x)
    plot_df <- data_frame(!!sym(name(x)) := as.Date(as.vector(x)))

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
#' @export
autoplot.NumericVariable <- function(x, ...) {
    v <- as.vector(x)
    plot_df <- data_frame(!!sym(name(x)) := v)
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

#' @importFrom crunch  alias datasetReference loadDataset
#' @importFrom ggplot2 autoplot
plotCategorical <- function(x, ...) {
    ds <- loadDataset(datasetReference(x))
    cube <- crtabs(paste0("~", alias(x)), ds)
    autoplot(cube, ...)
}

plot_fun_lookup <- function(plot_dim, plot_type) {
    plot_fun <- paste0("crunch_", plot_dim, "d_", plot_type, "_plot")
    return(get(plot_fun))
}

#' Autoplot methods for Crunch Objects
#' 
#' Generates ggplot representations of CrunchVariables and CrunchCubes
#' 
#' The Crunch autoplot methods generate plots which are optimized for various crunch objects.
#' This allows you to visualize the object without bringing it into memory. You can select
#' between three families of plots which will attempt to accomodate the dimensionality of
#' the plotted object. These plots can be further extended and customized with other ggplot methods. 
#' 
#' @param x a CrunchCube, or CrunchVariable
#' @param plot_type One of `"dot"`, `"grid"`, or `"bar"` which indicates the plot family
#' you would like to use. Higher dimensional plots add color coding or facets depending
#' on the dimensionality of the data. 
#' @param measure The measure you wish to plot. This will usually be `"count"`, the default
#' but can also be `".unweighted_counts"` or any other measure stored in the cube.
#' @param ... ignored
#' 
#' @export
#' @name autoplot
#' @importFrom rlang !! !!! .data sym syms
#' @importFrom purrr map map_chr
#' @importFrom dplyr mutate filter pull
#' @importFrom ggplot2 ggtitle
autoplot.CrunchCube <- function(x, 
    plot_type = c("dot", "grid", "bar"), 
    measure = "count") {
    plot_type = match.arg(plot_type)
    display_names <- map(x@dims, "references") %>% 
        map_chr("name")
    
    if (length(measure) > 1) {
        # TODO think about how plots can support more than one measure. 
        # for instance measures could be the grouping variable for 2d dot plots
        stop("Autoplot can only support one measure.", .call = FALSE)
    }
    measure <- sym(measure)
    
    # TODO do we want to make this optional to allow people to plot missing entries?
    plot_tbl <- as_tibble(x) %>% 
        filter(!.data$is_missing) 
    
    # Select the dimension columns from the table, this is necessary because the
    # names in the tibble are unique, while the cube dimnames are not. 
    dim_names <- names(plot_tbl)[seq_len(length(x@dims))] 
    
    # Only include rows where the MR selection dimensions are TRUE
    mr_selection_vars <- dim_names[getDimTypes(x) == "mr_selections"]
    if (length(mr_selection_vars)) {
        plot_tbl <- plot_tbl %>% 
            filter(!!!syms(mr_selection_vars))
    }
    #drop the MR selection dimensions for plotting
    dims <- syms(setdiff(dim_names, mr_selection_vars)) 
    
    plot_fun <- plot_fun_lookup(min(2, length(dims)), plot_type)

    out <- plot_fun(plot_tbl, dims, measure, display_names) +
        theme_crunch() +
        labs(title = paste0(unique(display_names), collapse = " + "),
            subtitle = x@dims[[1]]$references$description)

    if (plot_type == "grid") {
        # This is here instead of in the 2d_grid plot function because theme_crunch
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

#' @importFrom dplyr arrange desc mutate
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

# 1d_grid plots are equivalent to dot plots, so we redirect this call
crunch_1d_grid_plot <- function(...) crunch_1d_dot_plot(...)

#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes geom_point ggplot
crunch_2d_grid_plot <- function(tibble, dims, measure, display_names) {
    tibble %>% 
        mutate(!!measure := ifelse(!!measure == 0, NA, !!measure)) %>% 
        ggplot(aes(x = !!dims[[1]], y = !!dims[[2]], size = !!measure)) +
        geom_point() 
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