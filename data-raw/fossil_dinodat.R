fossil_dinodat_raw <- readRDS("data-raw/fossil_dinodat.RDS")


# remove metadata and add sample_id
fossil_dinodat <- select(fossil_dinodat_raw, -c(depth, method, Age_Ma)) |> 
  mutate(
    sample_id = row_number(),
    longitude = 144.412700,
    latitude = -42.609680
  )

# metadata
fossil_dinodat_meta <- select(fossil_dinodat_raw, c(depth, method, Age_Ma)) |> 
  mutate(sample_id = row_number())

use_data(fossil_dinodat)
use_data(fossil_dinodat_meta)
