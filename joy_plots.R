library(tidyverse)
library(ggridges)
library(here)

source(here("functions","make_joy_data.R"))

dimensions <- 80
rel_min_height <- 1/dimensions

joy <- joy_df(dimensions = dimensions)

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

# trying out smooth areas with jagged edges ----
# likely there is a better way of doing this
joy_test <- 
  joy %>%
  mutate(elev_test = case_when(
    lat < -9.91 ~ runif(6400, min = 156, max = 1678),
    lat > -9.79 ~ runif(6400, min = 156, max = 1678),
    TRUE ~ elev
  ))

classic_2 <- ggplot(joy_test, aes(lat, long, group = long, height = elev_test)) +
  geom_density_ridges(stat = "identity",
                      scale = 8, 
                      fill = "black",
                      color = "white",
                      rel_min_height = 0.03) + 
  my_theme()
classic_2

ggsave(here("outputs","classic_2.png"), classic_2, units = "px",
       width = 2500, height = 2500, dpi = 320)


joy_test <- 
  joy %>%
  mutate(elev_test = case_when(
    lat < -9.91 ~ runif(6400, min = 156, max = 1678),
    lat > -9.79 ~ runif(6400, min = 156, max = 1678),
    TRUE ~ elev
  ))

ggplot(joy_test, aes(lat, long, group = long, 
                height = elev_test, fill = elev_test)) +
  geom_density_ridges_gradient(stat = 'identity', 
                               scale = 4, 
                               show.legend = FALSE,
                               rel_min_height = 0.0005) +
  scale_fill_viridis_c(option = "E") + 
  my_theme(fill = "lightblue")

# Testing out new functions using noise from ambient
# test
make_noise_elev(dimension = 30, noise_type = "worl", 
                seed = 19, oct = 4, freq = 0.05) %>%
  joy_df_noise(multiple = 1) %>%
  ggplot(aes(lat, long, group = long, height = elev)) +
  geom_density_ridges(stat = "identity",
                      scale = 15, 
                      fill = "black",
                      color = "white",
                      rel_min_height = 0.01) + 
  my_theme()

make_noise_elev(dimension = 87, noise_type = "perlin", 
                seed = 19, oct = 4, freq = 0.05) %>%
  joy_df_noise(multiple = 1) %>%
  ggplot(aes(lat, long, group = long, height = elev)) +
  geom_density_ridges(stat = "identity",
                      scale = 20, 
                      fill = "black",
                      color = "white",
                      rel_min_height = 0.001) + 
  my_theme() -> perlin_joy_1
perlin_joy_1

ggsave(here("outputs","perlin_joy_1.png"), perlin_joy_1, units = "px",
       width = 2500, height = 2500, dpi = 320)

# can I plot two types of noise together on the same plot?
# can I colour the lines randomly? 
