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


