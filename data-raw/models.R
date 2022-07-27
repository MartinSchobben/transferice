library(recipes)
library(rsample)
library(dplyr)

set.seed(2)
# splitting
modern_split <- initial_split(modern, prop = 0.8, strata = "latitude")
# training
modern_train <- training(modern_split)
# testing
modern_test <- testing(modern_split)

# variables
vars <- role_organizer(modern, "t_an")
# taxa
txa <- vars[names(vars) == "predictor"] |> unname()

# recipe
tuned_recipe <- recipe(x = modern, vars = vars, roles = names(vars)) |> 
  # rescale predictor
  step_log(dplyr::any_of(txa), offset = 0.025) |> 
  # center predictors
  step_center(dplyr::any_of(txa)) |> 
  # reduce dimensions
  step_pca(dplyr::any_of(txa), num_comp = tune::tune()) |> 
  # remove location that lie close to each other
  step_zerogeodist(lon = longitude, lat = latitude, skip = TRUE) |> 
  # update spatial to predictor
  recipes::update_role(longitude, latitude, new_role = "predictor")


library(nlme)
library(parsnip)
library(multilevelmod)

# model
gls_model <- linear_reg() |> 
  set_engine(
    "gls",  
    control = nlme::lmeControl(opt = 'optim'),
    correlation = nlme::corSpatial(
      form = ~longitude|latitude, 
      type = "g", 
      nugget = TRUE
    )
  )  |> 
  # usage of the model for regression
  set_mode('regression')

# Workflow for training

library(workflows)
library(tune)
library(yardstick)

# fixed formula
fx <- formula_parser(modern, "t_an", exclude = c("longitude", "latitude"))

# workflow
modern_workflow <- workflow() |> 
  add_recipe(tuned_recipe) |> 
  add_model(gls_model, formula = fx) 

# multiple cross validation
set.seed(3)
modern_cv <- vfold_cv(
  training(modern_split), 
  v = 10, 
  strata = "latitude"
)

# tuning grid
tune_grid <- dials::grid_regular(
  tune::extract_parameter_set_dials(modern_workflow), 
  levels = 4
)

# tuning
tuned_workflow <- tune::tune_grid(
  modern_workflow,
  resamples = modern_cv,
  grid = tune_grid,
  metrics = yardstick::metric_set(yardstick::rmse),
  control = ctrl
)

# select model with 4 PCs
final_workflow <- tune::finalize_workflow(
  modern_workflow,
  tibble::tibble(num_comp = 4)
)

# fit the final model (does not need to be tuned)
validation_modern <- tune::last_fit(final_workflow, split = modern_split)
final_fit <- fit(final_workflow, data = modern)

# parameters
parms <- c("temperature", "phosphate", "nitrate", "silicate", "oxygen", 
           "salinity", "density")

# meta
meta <- c("sample_id", "sample_depth_mbsf", "hole_id", "site_hole", 
          "longitude", "latitude", "source_citation", "age_ma")

# internal data
use_data(tuned_recipe, modern_split, modern_workflow, tuned_workflow, final_workflow, 
         validation_modern, final_fit, parms, meta, internal = TRUE, 
         overwrite = TRUE)
