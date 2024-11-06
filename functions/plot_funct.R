my_theme <- function(fill = "black", font = "Avenir", size = 16) {
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = fill),
        axis.line = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = fill),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(colour = "white",
                                  family = font,
                                  size = 16,
                                  hjust = 0.5, vjust = 0.5))
}

ridge_classic <- function(df, x, y, height,
                          scale = 0.2, linewidth = 1,
                          fill = "black",
                          colour = "white") {
  ggplot(df, aes(
    x = {{x}}, y = {{y}},
    height = {{height}},
    group = {{y}}
  )) +
    geom_ridgeline(
      min_height = min(df$height),
      scale = scale,
      linewidth = linewidth,
      fill = fill,
      colour = colour) +
    scale_y_reverse()
}

density_ridge <- function(df, x, y, height,
                          linewidth = 1.2, scale = 18,
                          min_height = -0.2,
                          fill = "black",
                          colour = "white") {
  
  ggplot(df, aes(
    x = {{x}}, y = {{y}},
    height = {{height}},
    group = {{y}}
  )) +
    geom_density_ridges(stat = "identity",
                        linewidth = linewidth,
                        scale = scale, 
                        fill = fill,
                        color = colour,
                        rel_min_height = min_height
    )
  
}

# functions for data pulled from geo data
# make joy data has function for making region and df data

# standard black and white plot
joy_standard <- function(region_data, df,
                         min_height = 0,
                         linewidth = 0.75,
                         scale = 25) {
  ggplot() +
    geom_sf(
      data = region_data, 
      colour = NA, fill = NA
    ) +
    geom_ridgeline(
      data = df, aes(
        x = x, y = y,
        group = y,
        height = elev
      ),
      scale = scale,
      fill = "black",
      colour = "white",
      min_height = min_height,
      linewidth = linewidth
    )
}

# plot using cont colours along rows
joy_colour <- function(region_data, df, pal,
                       min_height = 0,
                       linewidth = 0.75,
                       scale = 25) {
  
  df <- df %>%
    mutate(class = cut_number(y, n = length(pal)))
  
  ggplot() +
    geom_sf(data = region_data, colour = NA, fill = NA) +
    geom_ridgeline(
      data = df, aes(
        x = x, y = y, 
        group = y, 
        height = elev,
        colour = class
      ),
      min_height = min_height,
      scale = scale,
      fill = "black",
      linewidth = linewidth,
      show.legend = FALSE
    ) +
    scale_colour_manual(values = alpha(pal), 0.75)
}




