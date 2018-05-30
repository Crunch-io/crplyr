crunch_colors <- c(
    "#316395", "#cf3e3e", "#fcb73e", "#37ad6c", "#9537b5", "#17becf", 
    "#e377c2", "#fdae6b", "#0099c6", "#ed5487", "#3366cc"
    )
card_colors <- c("#d11141","#007F65")

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
    (theme_minimal() +
            theme(
                line = element_line(colour = "black"),
                axis.title = element_blank(),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold", color = "#007F65"),
                plot.margin = unit(c(1, 1, 1, 1), "lines"),
                strip.background = element_rect())
    )
}

#' @importFrom viridis viridis
#' @importFrom dplyr summarize pull
generate_colors <- function(tibble, dim) {
    n_cols <- tibble %>% 
        summarize(cols = length(unique(!!dim))) %>% 
        pull(cols)
    if (n_cols > length(crunch_colors)) {
        return(c(crunch_colors, viridis(n_cols - length(crunch_colors))))
    } 
    return(crunch_colors)
}

#' @importFrom ggplot2 aes autoplot geom_histogram ggplot ggtitle
#' @importFrom crunch name
autoplot.NumericVariable <- function(x) {
    v <- as.vector(x)
    binwidth <- round((max(v) - min(v)) / 5, 0)
    ggplot() +
        aes(v) +
        geom_histogram(binwidth = binwidth, fill = crunch_colors["dark_green"]) +
        theme_crunch() +
        ggtitle(name(x))
}

#' @export
autoplot.CategoricalVariable <- function(x) plotCategorical(x)

#' @export
autoplot.CategoricalArrayVariable <- function(x) plotCategorical(x)

#' @export
autoplot.MultipleResponseVariable <- function(x) plotCategorical(x)
#' @importFrom crunch datasetReference loadDataset
#' @importFrom ggplot2 autoplot
plotCategorical <- function(x) {
    ds <- loadDataset(datasetReference(x))
    cube <- crtabs(paste0("~", alias(x)), ds)
    autoplot(cube)
}

#' @export
#' @importFrom rlang sym syms
#' @importFrom purrr map map_chr
#' @importFrom dplyr mutate filter pull
#' @importFrom ggplot2 ggtitle
autoplot.CrunchCube <- function(x, plot_type = c("dot", "grid", "bar"), measure = "count") {
    display_names <- map(x@dims, "references") %>% 
        map_chr("name")
    measure <- sym(measure)
    plot_tbl <- as_tibble(x) %>% 
        filter(!is_missing) 
    
    dim_names <- names(plot_tbl)[1:length(x@dims)] #Select the dimension columns from the table

    # Remove non-selected MR vars
    mr_selection_vars <- dim_names[getDimTypes(x) == "mr_selections"]
    if (length(mr_selection_vars)) {
        plot_tbl <- plot_tbl %>% 
            filter(!!!syms(mr_selection_vars))
    }

    dims <- syms(setdiff(dim_names, mr_selection_vars)) #drop the MR selection dimensions for plotting
    
    plot_lookup <- as_tibble(expand.grid(dims = 1:2, type = c("bar", "dot", "grid"))) %>% 
        mutate(plot_function = paste0("crunch_", dims, "d_", type, "_plot"))
    plot_dim <- min(2, length(dims))
    f <- plot_lookup %>% 
        filter(dims == plot_dim, type == plot_type) %>% 
        pull(plot_function) %>% 
        get()
    out <- f(plot_tbl, dims, measure, display_names) +
        theme_crunch() +
        ggtitle(paste0(unique(display_names), collapse = " + "))
    
    if (plot_type == "grid") {
        # This is here instead of in the 2d_grid plot function because theme_crunch
        # includes an axis.text specification. 
        out <- out + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    # If there are more than two dimensions, just keep adding facets
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
crunch_1d_grid_plot <- function(...) crunch_1d_dot_plot(...)

crunch_2d_grid_plot <- function(tibble, dims, measure, display_names) {
    tibble %>% 
        mutate(!!measure := ifelse(!!measure == 0, NA, !!measure)) %>% 
        ggplot(aes(x = !!dims[[1]], y = !!dims[[2]], size = !!measure)) +
        geom_point() 
}

#' @importFrom ggplot2 ggplot geom_point labs scale_color_manual
crunch_2d_dot_plot <- function(tibble, dims, measure, display_names) {
    cols <- generate_colors(tibble, dims[[2]])
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
    cols <- generate_colors(tibble, dims[[2]])
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

#' @importFrom dplyr summarize
.crunch_2d_tibble <- function(tibble, dims, measure) {
    levs <- tibble %>% 
        group_by(!!dims[[1]]) %>% 
        summarize(order_var = sum(!!measure)) %>% 
        arrange(order_var) %>% 
        pull(!!dims[[1]])
    
    out <- tibble %>% 
        mutate(!!dims[[1]] := factor(!!dims[[1]], levels = levs)) 
    return(out)
}
#' @importFrom ggplot2 facet_wrap facet_grid vars
add_facets <- function(plot, dims) {
    n_dims <- length(dims)
    if (n_dims == 1) {
        plot +
            facet_wrap(vars(!!dims[[1]]))
    } else if (n_dims == 2) {
        plot +
            facet_grid(vars(!!dims[[1]]), vars(!!dims[[2]]))
    } else {
        stop("Too many dimensions to plot, try subsetting the CrunchCube")
    }

}
