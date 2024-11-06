# good guide: https://dieghernan.github.io/202205_Unknown-pleasures-R/

# Libraries ----

# Spatial
library(sf)
library(terra)
library(giscoR) # Shapes
library(elevatr)

# Data viz and wrangling
library(ggplot2)
library(dplyr)
library(ggridges)
library(MetBrewer)

# other
library(units)
library(showtext)

# pull plotting functions for ease ----
source("functions/plot_funct.R")
source("functions/make_joy_data.R")

# grab some fonts
# https://fonts.google.com/
font_add_google(
  c(
    "Source Code Pro"
  )
)
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

f1 = "Source Code Pro"

# get regional data ----
# to find nuts id, do something like
# gisco_get_nuts(country = "UK") or look at gisco package page

r <- region_data(nuts_id = "UKJ11") # testing Berkshire

# setup the plotting data for area ----
d <- dem_data(region = r, zoom = 7, row_target = 90)


# make standard plot ----
joy_standard(region_data = r, df = d, scale = 25) +
  my_theme()

# coloured ----
# e.g. using met brewer - https://www.blakerobertmills.com/my-work/met-brewer
palette <- met.brewer("Hokusai1", n = 40)

( p1 <- joy_colour(region_data = r, df = d, pal = palette, 
           min_height = 0, linewidth = 1.5,
           scale = 25) +
  my_theme() )


# add title to say where this is
( p2 <- joy_standard(r, d) +
  labs(title = "Berkshire") +
  my_theme(size = 34, font = f1) )

joy_colour(region_data = r, df = d, pal = palette, 
           min_height = 0, linewidth = 1.5,
           scale = 25) +
  labs(title = "Berkshire") +
  my_theme(font = f1)


# save plots
ggsave("outputs/berkshire_bw.png", p2, dpi = 320,
       units = "px", width = 3000, height = 2500,
       device = ragg::agg_png)

ggsave("outputs/berkshire_colour.png", p1, dpi = 320,
       units = "px", width = 3000, height = 2500,
       device = ragg::agg_png)
