library(tidyverse)

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

# other rand generation for elev data
# http://uc-r.github.io/generating_random_numbers/
hist(runif(43, min = 0, max = 1000))
hist(sample(1:1000, size = 43, replace = T))
hist(rnorm(43, mean = 1000, sd = 500))
hist(runif(100, 0, 1000) |> rnorm(500, 100))
dnorm(1:100)
