library(purrr)
library(oceanexplorer)
library(sf)
library(tibble)
library(dplyr)
library(readxl)
library(janitor)
library(sf)

# read dinocyst data
dino_raw <- read_xlsx("data-raw/SurfaceDinos.xlsx") %>% janitor::clean_names()

# relative counts
dino_dat <- rowwise(dino_raw) %>%
  mutate(sum = sum(c_across(-c(station,depth, weight, lycopodium ,lyc_tab, longitude, latitude)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(-c(station,depth, weight, lycopodium ,lyc_tab, longitude, latitude), ~{./sum})) %>%
  select(-sum)


# parameters
params <- c("temperature", "phosphate", "nitrate", "silicate", "oxygen", 
            "salinity", "density")
# get NOAA annually averaged data for parameters on a 1 degree grid 
ls_data <- purrr::map(params, ~get_NOAA(.x, 1, "annual"))

# get locations
coord <- list()
coord$lon <- dino_dat$longitude
coord$lat <- dino_dat$latitude

# make sf collection
dino_dat <- st_as_sf(dino_dat, coords = c("longitude", "latitude"), crs = original_crs)

# get locations parameters
ls_params <- purrr::map(ls_data, ~filter_NOAA(.x, depth = 0,  coord = coord))

# make normal tibble
reduce_sf <- function(params) {
  
  # remove depth
  params <- purrr::map(params, ~select(.x, -.data$depth))
  
  # spatial join
  params <- purrr::reduce(params, sf::st_join)
  
  # loose units
  params <- mutate(params, t_an = units::drop_units(t_an))
  
  # remove geometry
  params <- select(params, -.data$geometry)

}

params_data <- reduce_sf(ls_params)


# all together
complete_data <- sf::st_join(dino_dat, params_data)

# save data
use_data(complete_data, overwrite = TRUE)
