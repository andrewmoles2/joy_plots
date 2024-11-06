# load libraries ----
library(tidyverse)
library(ggridges)
library(here)
library(MetBrewer)

# pull in data and plotting functions ----
source(here("functions", "plot_funct.R"))
source(here("functions","make_joy_data.R"))

# some setup ----
dimensions <- 80
rel_min_height <- 1/dimensions

joy <- joy_df(dimensions = dimensions)

# classic style ----
# calc rel_min_height with 1/n dimensions
classic_1 <- ggplot(joy, aes(lat, long, group = long, height = elev)) +
  geom_density_ridges(stat = "identity",
                      scale = 8, 
                      fill = "black",
                      color = "white",
                      rel_min_height = 0.03) + 
  my_theme()
classic_1

# colourful style ----
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

ggplot(joy_test, aes(lat, long, group = long, 
                height = elev_test, fill = elev_test)) +
  geom_density_ridges_gradient(stat = 'identity', 
                               scale = 4, 
                               show.legend = FALSE,
                               rel_min_height = 0.0005) +
  scale_fill_viridis_c(option = "E") + 
  my_theme(fill = "lightblue")

# Testing out new functions using noise from ambient ----
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

# test with colours
pal <- MetBrewer::met.brewer("Klimt", 30)
make_noise_elev(seed = 1989, noise_type = "simp",
                dimension = 50) %>%
  joy_df_noise() %>%
  mutate(class = cut_number(long, n = length(pal))) %>%
  ggplot(aes(lat, long, group = long, height = elev, colour = class)) +
  geom_density_ridges(stat = "identity",
                      fill = "black",
                      scale = 10,
                      linewidth = 0.3,
                      show.legend = FALSE) +
  scale_colour_manual(values = alpha(pal), 0.85) +
  my_theme() -> simplex_joy_1
simplex_joy_1

ggsave("outputs/simplex_joy_1.png", simplex_joy_1, dpi = 320,
       units = "px", width = 3000, height = 2500,
       device = ragg::agg_png)

# testing out new joy function ----
joy_df_noise_new(columns = 100, rows = 60) %>%
  density_ridge(df = ., x = col, y = row, height = height,
                scale = 4.5, min_height = -0.005) + 
  my_theme()

joy_df_noise_new()

# unfiltered - using ridgeline default
joy_df_noise_new() %>%
  ridge_classic(x = col, y = row, height = height,
                scale = 0.9, linewidth = 0.45) +
  my_theme()

joy_df_noise_new() %>%
  ggplot(aes(x = col, y = row, colour = height)) +
  geom_point() +
  my_theme()

joy_df_noise_new(columns = 80, rows = 40) %>%
  mutate(height = ifelse(col %in% 35:55, abs(height * 4.2), height)) %>%
  mutate(height = ifelse(col %in% 10:20, abs(height * 1.5), height)) %>%
  mutate(height = ifelse(col %in% 61:73, abs(height * 1.2), height)) %>%
  ridge_classic(x = col, y = row, height = height,
                scale = 0.6, linewidth = 0.45) +
  my_theme()


sample(1:40, 8)
sample(1:80, 20)

joy_df_noise_new(columns = 150, rows = 60) %>%
  mutate(height = ifelse(row %in% sample(1:40, 20) & col %in% sample(1:80, 20), 
                         abs(height * 3), height)) %>%
  ridge_classic(x = col, y = row, height = height,
                scale = 0.6, linewidth = 0.45) +
  my_theme()

# replicate joy div plot ----
# https://datawookie.dev/blog/2019/07/recreating-unknown-pleasures-graphic/

pulsar <- read.csv("https://gist.githubusercontent.com/borgar/31c1e476b8e92a11d7e9/raw/0fae97dab6830ecee185a63c1cee0008f6778ff6/pulsar.csv", header = FALSE)

pulsar_tidy <- pulsar %>%
  mutate(row = row_number()) %>%
  pivot_longer(-row, names_to = "col", values_to = "height") %>%
  mutate(
    col = sub("V", "", col) %>% as.numeric(),
  )

pulsar_tidy %>%
  ggplot(aes(x = col, y = row, colour = height)) +
  geom_point() +
  theme_minimal()

# write out files for later use and examples
#write_csv(pulsar, file = "data/pulsar.csv")
#write_csv(pulsar_tidy, file = "data/pulsar_long.csv")

ridge_classic(pulsar_tidy, x = col, y = row, height = height,
              fill = "#DD33FF", colour = "#FFDD33") + 
  my_theme(fill = "#31FFDD")

density_ridge(pulsar_tidy, x = col, y = row, height = height) +
  my_theme()

ridge_classic(pulsar_tidy, x = col, y = row, height = height,
              fill = "#20a39e", colour = "#ffba49") + 
  my_theme(fill = "#20a39e")

# side of plot
df_side <- array()

for (i in 1:50) {
  
  x <- runif(80, -2.1, 0.71)
  
  df_side <- cbind(df_side, x)
}

df_side <- as.data.frame(df_side)
df_side <- df_side[, -1]

# peaks
df_peak <- array()

for (i in 1:50) {
  
  x <- runif(80, -0.2, 42.1)
  
  df_peak <- cbind(df_peak, x)
}

df_peak <- as.data.frame(df_peak)
df_peak <- df_peak[, -1]

# between
df_mid <- array()

for (i in 1:50) {
  
  x <- runif(80, -0.2, 20.1)
  
  df_mid <- cbind(df_mid, x)
}

df_mid <- as.data.frame(df_mid)
df_mid <- df_mid[, -1]

# bring together
df_list <- list(df_side, df_mid, df_peak, df_mid, df_side)

df <- reduce(df_list, cbind)
colnames(df) <- paste0("V", 1:ncol(df))

df_long <- df %>%
  mutate(row = row_number()) %>%
  pivot_longer(-row, names_to = "col", values_to = "height") %>%
  mutate(
    col = sub("V", "", col) %>% as.numeric(),
  )

df_long %>%
  filter(col %in% sample(col, size = 200)) %>%
  density_ridge(x = col, y = row, height = height,
                scale = 1.5, min_height = -0.075,
                linewidth = 0.5) + 
  my_theme()


