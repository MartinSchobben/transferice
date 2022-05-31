# spatial interpolation
interpolate_model <- function(origin, output, y, base_map) {
  
  # y <- rlang::enquo(y)
  
  comb <- dplyr::bind_cols(origin, output)
  
  # make coordinate sf object
  crds <- sf::st_as_sf(
    comb,
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  
  # remove duplicate locations with tolerance 250 km radius
  d <- sf::st_is_within_distance(crds, dist = 2500)
  dupl <- unlist(mapply(function(x,y) x[x < y], d, seq_along(d)))
  crds <- crds[-dupl, ]
  
  # estimate
  est <- paste0(".pred_",  y)
  # formula
  fml <- as.formula(paste0(est, "~", y))
  # nugget based on the RMSE
  mean_var <- yardstick::rmse(comb, !!rlang::sym(y), !!rlang::sym(est))$.estimate 
  #  partial sill based on the variance of the outcome
  tot_var <- var(dplyr::pull(comb, !!rlang::sym(y)))
  
  # variogram
  vario <- gstat::variogram(fml, crds) 
  
  # variogram model fit
  vario_model <- gstat::fit.variogram(
    vario, 
    gstat::vgm(tot_var, "Sph", 700, mean_var)
  )
  
  # model
  g <- gstat::gstat(formula = fml, data = crds, model = vario_model)
  # interpolate on base grid
  z <- predict(g, base_map)
  z = z["var1.pred",,] # extract prediction
  names(z) = est # rename
  z
}