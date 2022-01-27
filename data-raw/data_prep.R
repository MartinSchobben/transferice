library(purrr)
library(oceanexplorer)
library(sf)
library(tibble)
library(dplyr)
library(readxl)
library(janitor)
library(sf)
library(dplyr)


# read dinocyst data
dino_raw <- read_xlsx("data-raw/SurfaceDinos.xlsx") %>% janitor::clean_names()

# make sf collection (GPS coords are probably)
dino_raw <- st_as_sf(dino_raw, coords = c("longitude", "latitude"), crs = 4326)

# meta
meta <- c("station", "depth", "weight", "lycopodium", "lyc_tab", "geometry")

# zeros are real zeros in counts hence replace NAs with 0
dino_raw <- dino_raw %>% 
  dplyr::mutate(across(-any_of(meta), ~tidyr::replace_na(.x, 0)))

# relative counts in sample
dino_rel <- rowwise(dino_raw) %>%
  mutate(sum = sum(c_across(-any_of(meta)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(-any_of(meta), ~{./sum})) %>%
  select(-sum)

# prep
# remove zero columns
dino_rel <- dplyr::select(
  dino_rel, 
  any_of(meta), 
  where(~is.numeric(.x) && sum(.x, na.rm = TRUE) > 0)
  )


# logit trans (warning: proportions remapped to (0.025, 0.975))
dino_rel <- dplyr::mutate(dino_rel, dplyr::across(-any_of(meta), car::logit))

# parameters
params <- c("temperature", "phosphate", "nitrate", "silicate", "oxygen", 
            "salinity", "density")
# get NOAA annually averaged data for parameters on a 1 degree grid 
ls_data <- purrr::map(params, ~get_NOAA(.x, 1, "annual"))
NOAA_crs <- st_crs(ls_data[[1]]) # store crs
# make projections comparable
dino_rel <- st_transform(dino_rel, crs = NOAA_crs)


# get locations
dino_coords <-dino_rel$geometry %>% st_coordinates() 
coord <- list()
coord$lon <- dino_coords[, 1] %>% unname()
coord$lat <- dino_coords[, 2] %>% unname()

# get locations parameters
ls_params <- purrr::map(ls_data, ~filter_NOAA(.x, depth = 0,  coord = coord))

# make sf collection (GPS coords are probably)
dino_rel <- st_as_sf(dino_rel, coords = c("longitude", "latitude"), crs = 4326)
# make projections comparable
dino_rel <- st_transform(dino_rel, crs = NOAA_crs)

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
complete_data <- sf::st_join(dino_rel, params_data)

# save data
use_data(complete_data, overwrite = TRUE)

# relative dinos
use_data(dino_rel, overwrite = TRUE)

# vectors of variables
parms <- colnames(complete_data)[grepl("_an$", colnames(complete_data))] %>% 
  setNames(
    nm = c("temperature", "phosphate", "nitrate", "silicate", "oxygen",
           "salinity", "density")
    )
use_data(parms, overwrite = TRUE)
meta <- c("station","depth","weight","lycopodium","lyc_tab", "geometry")
use_data(meta, overwrite = TRUE)
dino <- colnames(complete_data)[!colnames(complete_data) %in% c(parms, meta)]
use_data(dino, overwrite = TRUE)