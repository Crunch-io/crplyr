crunch_colors <- c(
    light_green  = "#d11141",
    dark_green   = "#007F65",
    gray_blue    = '#316395', 
    red          = '#cf3e3e', 
    goldenrod    = '#fcb73e', 
    bright_green = '#37ad6c',
    fuscia       = '#9537b5', 
    tourquoise   = '#17becf', 
    pink         = '#e377c2', 
    peach        = '#fdae6b', 
    light_blue   = '#0099c6',
    hot_pink     = '#ed5487', 
    royal_blue   = '#3366cc', 
    yellow_green = '#aaaa11'
    )
crunch_cols <- function(...){
    cols <- c(...)
    if (is.null(cols)) {
        return(crunch_colors)
    }
    return(crunch_colors[cols])
}

crunch_palettes <- list(
    main = crunch_colors[3:5],
    cards = crunch_colors[c("dark_green", "light_green")]
)


crunch_palette <- function(palette = "main", reverse = FALSE, ...) {
  pal <- crunch_palettes[[palette]]
  if (reverse) {
      pal <- rev(pal)
  }
  colorRampPalette(pal, ...)
}

scale_fill_crunch <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- crunch_palette(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("fill", paste0("crunch_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' Crunch ggplot theme
#' 
#' Style ggplots according to Crunch style. 
#'
#' @param base_size Base text size
#' @param base_family 
#'
#' @export
#' 
#' @importFrom ggplot2 theme_minimal theme
#'
#' @examples
#' qplot(mtcars$mpg) +
#'    theme_crunch()
theme_crunch <- function(base_size = 12, base_family = "sans") {
    (theme_minimal()+
            theme(
                line = element_line(colour = "black"),
                axis.title = element_blank(),
                axis.text = element_text(),
                axis.ticks = element_blank(),
                axis.line = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold", color = crunch_colors["dark_green"]),
                plot.margin = unit(c(1, 1, 1, 1), "lines"),
                strip.background = element_rect())
        )
}


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

plotCategorical <- function(x) {
    ds <- loadDataset(datasetReference(x))
    cube <- crtabs(paste0("~", alias(x)), ds)
    autoPlot(cube)
}

#' @export
autoplot.CrunchCube <- function(x, plot_type = c("dot", "grid", "bar"), measure = "count") {
    dims <- syms(names(dimnames(x)))
    measure <- sym(measure)
    tibble <- as_tibble(cube) %>% 
        filter(!is_missing) 
    
    plot_lookup <- as_tibble(expand.grid(dims = 1:2, type = c("bar", "dot", "grid"))) %>% 
        mutate(plot_function = paste0("crunch_", dims, "d_", type, "_plot"))

    f <- plot_lookup %>% 
        filter(dims == min(2, length(dims)), type == type) %>% 
        pull(plot_function) %>% 
        get()
    out <- f(tibble, dims, measure) +
        theme_crunch() +
        ggtitle(paste0(names(dimnames(x)), collapse = " + ")) +
        scale_fill_crunch()
    
    # If there are more than two dimensions, just keep adding facets
    if (length(dims) > 2) {
        add_facets(out, dims[[3:length(dims)]])
    }
    return(out)
}

.crunch_1d_tibble <- function(tibble, dims, measure) {
    tibble %>% 
        arrange(desc(!!measure)) %>% 
        mutate(!!dims[[1]] := reorder(!!dims[[1]], !!measure))
}

crunch_1d_dot_plot <- function(tibble, dims, measure){
    .crunch_1d_tibble(tibble, dims, measure) %>% 
        ggplot(aes(y = !!dims[[1]], x = !!measure)) +
        geom_point()
}

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
    tibble %>% 
        ggplot(aes(
            x = !!measure, 
            y = !!dims[[1]], 
            group = !!dims[[2]], 
            colour = !!dims[[2]])) +
        geom_point()
}

crunch_2d_bar_plot <- function(tibble, dims, measure) {
    tibble %>% 
        ggplot(aes(
            x = !!dims[[1]], 
            y = !!measure,
            group = !!dims[[2]], 
            fill = !!dims[[2]])) +
        geom_bar(stat = "identity") +
        coord_flip()
}

add_facets <- function(plot, dims) {
    plot +
        facet_wrap(vars(!!!dim))
}
