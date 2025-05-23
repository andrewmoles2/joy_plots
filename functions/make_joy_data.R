library(tidyverse)

# first attempt - jagged edges, not realistic but fun
joy_df <- function(dimensions = 43, lat_n = -10, long_n = 10,
                   elev_mean = 5000, elev_sd = 1500, seed = 1234,
                   lat_dif = 0.30, long_dif = 0.180, trim_edge = TRUE) {
  
  set.seed(seed)
  
  lat <- seq(lat_n, (lat_n - lat_dif), length.out = dimensions)
  long <- seq(long_n, (long_n + long_dif), length.out = dimensions)
  elev <- rnorm(dimensions, mean = elev_mean, sd = elev_sd)
  
  int_df <- data.frame(elev, lat, long)
  
  final_df <- int_df |>
    dplyr::group_by(long) |>
    dplyr::summarise(lat = lat <- seq(lat_n, (lat_n + lat_dif), length.out = dimensions),
                     elev = rnorm(dimensions, mean = elev_mean, sd = elev_sd)) |>
    dplyr::ungroup()
  
  # trim edge - T or F
  if (trim_edge == TRUE) {
  
    final_df <- final_df |>
      dplyr::mutate(elev = ifelse(lat < min(lat)+0.01, 0, elev),
                    elev = ifelse(lat > max(lat)-0.01, 0, elev)) |>
      dplyr::mutate(elev = ifelse(elev == 0, runif(10, min = 0, max = (elev_mean/elev_sd)*100), elev))
  
    return(final_df)
    
  } else {
    return(final_df)
  }
}

# second attempt - using noise functions makes for cleaner finish
# requires a bit more functional work

# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/match.arg
# https://ambient.data-imaginist.com/reference/index.html
library(ambient)

make_noise_elev <- function(dimension = 100, seed = 1234, freq = 0.04, oct = 3,
                        noise_type = c("perlin", "simplex", "value", "cubic", "worley")) {
  
  set.seed(seed)
  
  noise_type <- match.arg(noise_type)
  
  noise_grid <- switch(noise_type,
                       perlin = noise_perlin(dim = c(dimension, dimension), frequency = freq, octaves = oct) |> 
                         as.data.frame(),
                       simplex = noise_simplex(dim = c(dimension, dimension), frequency = freq, octaves = oct) |> 
                         as.data.frame(),
                       value = noise_value(dim = c(dimension, dimension), frequency = freq, octaves = oct) |> 
                         as.data.frame(),
                       cubic = noise_cubic(dim = c(dimension, dimension), frequency = freq, octaves = oct) |> 
                         as.data.frame(),
                       worley = noise_worley(dim = c(dimension, dimension), frequency = freq, octaves = oct, value = 'distance', distance = 'natural') |> 
                         as.data.frame())
  
  noise_elev <- unname(unlist(noise_grid[sample(nrow(noise_grid), 1),]))
  noise_elev <- abs(noise_elev)
  
  return(noise_elev)
}

joy_df_noise <- function(x, seed = 1234,
                         lat_n = -10, long_n = 10,
                         lat_dif = 0.30, long_dif = 0.180,
                         multiple = 1000) {
  
  set.seed(seed)
  
  lat <- seq(lat_n, (lat_n - lat_dif), length.out = length(x))
  long <- seq(long_n, (long_n + long_dif), length.out = length(x))
  elev <- abs(x * multiple)
  
  int_df <- data.frame(elev, lat, long)
  
  final_df <- int_df |>
    dplyr::group_by(long) |>
    dplyr::reframe(lat = lat <- seq(lat_n, (lat_n + lat_dif), length.out = length(x)),
                   elev = x * multiple) |>
    dplyr::ungroup()
  
  return(final_df)
}

# new version that starts as array -> pivoted long data frame 
# easier to control
joy_df_noise_new <- function(columns = 300, rows = 80) {
  
  noise_df <- array()
  
  for (i in 1:columns) {
    t <- sample(c("perlin", "simplex", "value", "cubic", "worley"), 1)
    f <- runif(1, min = 0.01, 0.06)
    o <- round(runif(1, min = 2, max = 4))
    n <- make_noise_elev(dimension = rows, seed = i, 
                         noise_type = t, freq = f, oct = o)
    noise_df <- cbind(noise_df, n)
  }
  
  noise_df <- as.data.frame(noise_df)
  noise_df <- noise_df[, -1]
  colnames(noise_df) <- paste0("V", 1:columns)
  
  noise_df <- noise_df %>%
    mutate(row = row_number()) %>%
    pivot_longer(-row, names_to = "col", values_to = "height") %>%
    mutate(
      col = sub("V", "", col) %>% as.numeric(),
    )
  
  return(noise_df)
  
}

# version that uses geo data (just EU for now)
region_data <- function(nuts_id = "UKH12") {
  # default is set to Cambridgeshire
  # to find nuts id, do something like
  # gisco_get_nuts(country = "UK")
  region <- gisco_get_nuts(nuts_id = nuts_id) %>%
    st_transform(25830)
  return(region)
}

australia_region_data <- function(state_name = "New South Wales") {
  # Get Australian states
  aus_states <- ne_states(country = "australia", returnclass = "sf")
  
  # Filter for the desired state
  region <- aus_states %>%
    filter(name == state_name) %>%
    st_transform(3577) # GDA2020 Australian Albers projection
  
  return(region)
}

dem_data <- function(region, 
                     zoom = 7, row_target = 90) {
  # zoom areas - https://wiki.openstreetmap.org/wiki/Zoom_levels 
  dem <- elevatr::get_elev_raster(
    region, z = zoom, clip = "bbox",
    expand = 10000
  ) %>%
    terra::rast() %>%
    terra::mask(terra::vect(region))
  names(dem) <- "elev"
  
  fact <- round(nrow(dem) / row_target)
  dem_agg <- aggregate(dem, fact)
  
  dem_agg[dem_agg < 0] <- 0
  dem_agg[is.na(dem_agg)] <- 0
  dem_df <- as.data.frame(dem_agg, xy = TRUE, na.rm = FALSE)
  
  return(dem_df)
}




