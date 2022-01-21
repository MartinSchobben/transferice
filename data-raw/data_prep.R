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

#meta
meta <- c("station", "depth", "weight", "lycopodium", "lyc_tab", "longitude", "latitude")

# zeros are real zeros in counts hence replace NAs with 0
dino_raw <- dino_raw %>% dplyr::mutate(across(-any_of(meta), ~tidyr::replace_na(.x, 0)))

# relative counts in sample
dino_rel <- rowwise(dino_raw) %>%
  mutate(sum = sum(c_across(-c(station, depth, weight, lycopodium ,lyc_tab, longitude, latitude)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(-any_of(meta), ~{./sum})) %>%
  select(-sum)

# normalized variables !!!!!check in future!!!!! maybe x - mean(x) / sqrt(x)
# https://davetang.org/muse/2014/07/07/quantile-normalisation-in-r/
quantile_normalisation <- function(df){
  df_rank <- apply(df,2,rank,ties.method="min")
  df_sorted <- data.frame(apply(df, 2, sort))
  df_mean <- apply(df_sorted, 1, mean)
  
  index_to_mean <- function(my_index, my_mean){
    return(my_mean[my_index])
  }
  
  df_final <- apply(df_rank, 2, index_to_mean, my_mean=df_mean)
  rownames(df_final) <- rownames(df)
  as_tibble(df_final)
}


# dino_norm <- bind_cols(
#   select(dino_raw, any_of(meta)),
#   quantile_normalisation(select(dino_raw, -any_of(meta)))  
#   )
 
#dino_norm <-mutate(dino_raw, across(-any_of(meta), scale))

# parameters
params <- c("temperature", "phosphate", "nitrate", "silicate", "oxygen", 
            "salinity", "density")
# get NOAA annually averaged data for parameters on a 1 degree grid 
ls_data <- purrr::map(params, ~get_NOAA(.x, 1, "annual"))
original_crs <- st_crs(ls_data[[1]]) # store crs

# get locations
coord <- list()
coord$lon <- dino_raw$longitude
coord$lat <- dino_raw$latitude

# get locations parameters
ls_params <- purrr::map(ls_data, ~filter_NOAA(.x, depth = 0,  coord = coord))

# make sf collection
dino_raw <- st_as_sf(dino_raw, coords = c("longitude", "latitude"), crs = original_crs)

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
complete_data <- sf::st_join(dino_raw, params_data)

# save data
use_data(complete_data, overwrite = TRUE)

# relative dinos
use_data(dino_rel, overwrite = TRUE)

# vectors of variables
parms <- colnames(complete_data)[grepl("_an$", colnames(complete_data))]
use_data(parms, overwrite = TRUE)
meta <- c("station","depth","weight","lycopodium","lyc_tab", "geometry")
use_data(meta, overwrite = TRUE)
dino <- colnames(complete_data)[!colnames(complete_data) %in% c(parms, meta)]
use_data(dino, overwrite = TRUE)