fossil_dinodat_raw <- readRDS("data-raw/fossil_dinodat.RDS")


# remove metadata and add sample_id
fossil_dinodat <- dplyr::select(fossil_dinodat_raw, -c(method, Age_Ma)) |> 
  dplyr::mutate(
    sample_id = dplyr::row_number(),
    longitude = 144.412700,
    latitude = -42.609680
  )

# get full dino names
meta <- pangaear::pg_data("10.1594/PANGAEA.779869") 
nms <- meta[[1]]$metadata$parameters[11:134] |> purrr::map_chr(list(1))

# filter for names
nms2 <- nms[stringr::str_detect(nms, "#")] |> purrr::map(~stringr::str_split(.x, "\\[#\\]")) |> purrr::flatten()
abbr <-purrr::map_chr(nms2, list(2)) |> stringr::str_remove_all("[\\(\\)]") |> stringr::str_trim()
names <- purrr::map_chr(nms2, list(1)) |> stringr::str_trim()
vc_nms <- rlang::set_names(abbr, names)

# rename function
fossil_dinodat <- fossil_dinodat[,-61] # remove duplicated name Hystrichokolpoma sp. 
fossil_dinodat <- fossil_dinodat |> 
  dplyr::rename_with(~stringr::str_remove(.x, "(?<=\\[#\\])(.)*")) |> 
  dplyr::rename_with(~stringr::str_trim(stringr::str_remove(.x, "\\[#\\]"))) |> 
  dplyr::rename(!!!vc_nms)
  
# metadata
fossil_dinodat_meta <- dplyr::select(fossil_dinodat_raw, c(depth, method, Age_Ma)) |> 
  dplyr::mutate(sample_id = dplyr::row_number())

use_data(fossil_dinodat, overwrite = TRUE)
use_data(fossil_dinodat_meta, overwrite = TRUE)

