library(tidyverse)
library(ggridges)
library(here)

source(here("functions","make_joy_data.R"))

joy <- joy_df(dimensions = 80)

my_theme <- function(fill = "black") {
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = fill),
        axis.line = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = fill),
        axis.ticks = element_blank(),
        axis.title = element_blank())
}

# classic style
# calc rel_min_height with 1/n dimensions
classic_1 <- ggplot(joy, aes(lat, long, group = long, height = elev)) +
  geom_density_ridges(stat = "identity",
                      scale = 8, 
                      fill = "black",
                      color = "white",
                      rel_min_height = 0.03) + 
  my_theme()
classic_1
# colourful style
colour_1 <- ggplot(joy, aes(lat, long, group = long, 
                        height = elev, fill = elev)) +
  geom_density_ridges_gradient(stat = 'identity', 
                               scale = 4, 
                               show.legend = FALSE,
                               rel_min_height = 0.0005) +
  scale_fill_viridis_c(option = "C") + 
  my_theme(fill = "lightblue")
colour_1
# save them
ggsave(here("outputs","classic_1.png"), classic_1, units = "px",
       width = 2500, height = 2500, dpi = 320)
ggsave(here("outputs","colourful_1.png"), colour_1, units = "px",
       width = 2500, height = 2500, dpi = 320)
