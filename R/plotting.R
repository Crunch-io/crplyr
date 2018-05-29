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
#' @param base_family 
#'
#' @export
#' 
#' @importFrom ggplot2 element_line element_blank element_text element_rect 
#' theme_minimal theme  rel unit 
#'
#' @examples
#' qplot(mtcars$mpg) +
#'    theme_crunch()
theme_crunch <- function(base_size = 12, base_family = "sans") {
    (theme_minimal() +
            theme(
                line = element_line(colour = "black"),
                axis.title = element_blank(),
                axis.text = element_text(),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold", color = "#007F65"),
                plot.margin = unit(c(1, 1, 1, 1), "lines"),
                strip.background = element_rect())
    )
}

#' @importFrom ggplot2 aes geom_histogram ggplot ggtitle
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
#' @importFrom dplyr mutate filter pull
#' @importFrom ggplot2 ggtitle
autoplot.CrunchCube <- function(x, plot_type = c("dot", "grid", "bar"), measure = "count") {
            browser()
    measure <- sym(measure)
    tibble <- as_tibble(cube) %>% 
        filter(!is_missing) 
    
    dim_names <- names(tibble)[1:length(dim(x))] #Select the dimension columns from the table
    mr_vars <- dim_names[getDimTypes(x) == "mr_selection"]
    
    dims <- syms(dim_names[getDimTypes(x) != "mr_selection"]) #drop the MR selection dimensions for plotting
    
    plot_lookup <- as_tibble(expand.grid(dims = 1:2, type = c("bar", "dot", "grid"))) %>% 
        mutate(plot_function = paste0("crunch_", dims, "d_", type, "_plot"))
    
    f <- plot_lookup %>% 
        filter(dims == min(2, length(dims)), type == plot_type) %>% 
        pull(plot_function) %>% 
        get()
    out <- f(tibble, dims, measure) +
        theme_crunch() +
        ggtitle(paste0(unique(names(dimnames(x))), collapse = " + ")) 
    # If there are more than two dimensions, just keep adding facets
    if (length(dims) > 2) {
        out <- add_facets(out, dims[3:length(dims)])
    }
    return(out)
}

#' @importFrom dplyr arrange desc mutate
.crunch_1d_tibble <- function(tibble, dims, measure) {
    tibble %>% 
        arrange(desc(!!measure)) %>% 
        mutate(!!dims[[1]] := reorder(!!dims[[1]], !!measure))
}

#' @importFrom ggplot2 geom_point ggplot
crunch_1d_dot_plot <- function(tibble, dims, measure){
    .crunch_1d_tibble(tibble, dims, measure) %>% 
        ggplot(aes(y = !!dims[[1]], x = !!measure)) +
        geom_point()
}

#' @importFrom ggplot2 coord_flip geom_bar ggplot
crunch_1d_bar_plot <- function(tibble, dims, measure){
    .crunch_1d_tibble(tibble, dims, measure) %>% 
        ggplot(aes(y = !!dims[[1]], x = !!measure)) +
        geom_bar(stat = "identity") +
        coord_flip()
}
crunch_1d_grid_plot <- function(...) crunch_1d_dot_plot(...)

crunch_2d_grid_plot <- function(tibble, dims, measure) {
    tibble %>% 
        mutate(!!measure := ifelse(!!measure == 0, NA, !!measure)) %>% 
        ggplot(aes(x = !!dims[[1]], y = !!dims[[2]], size = !!measure)) +
        geom_point()
}

crunch_2d_dot_plot <- function(tibble, dims, measure) {
    out <- tibble %>% 
        .crunch_2d_tibble(dims, measure) %>% 
        ggplot(aes(
            x = !!measure, 
            y = !!dims[[1]], 
            group = !!dims[[2]], 
            color = !!dims[[2]])) +
        geom_point() + 
        scale_color_manual(values = crunch_colors)
}

crunch_2d_bar_plot <- function(tibble, dims, measure) {
    tibble %>% 
        .crunch_2d_tibble(dims, measure) %>% 
        ggplot(aes(
            x = !!dims[[1]], 
            y = !!measure,
            group = !!dims[[2]], 
            fill = !!dims[[2]])) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = crunch_colors)
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
#' importFrom ggplot2 facet_wrap vars
add_facets <- function(plot, dims) {
    plot +
        facet_wrap(vars(!!!dims))
}
