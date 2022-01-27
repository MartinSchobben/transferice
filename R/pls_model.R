# (seed <- sample(2 ^ 31 - 1, 1))
# set.seed(seed)
# # remove missing env param data
# 
# dat <- as_tibble(complete_data) %>% tidyr::drop_na()
# dat <- dplyr::select(dat, -any_of(meta))
# 
# 
# # md1 <- plsr(t_an ~ ., data = select(dat, -c(p_an, n_an, i_an, o_an, s_an, I_an)), validation = "CV")
# rec <- plsr(cbind(t_an,p_an,n_an,i_an ,o_an,s_an,I_an) ~ ., data = dat, validation = "CV")
# 
# rec <- recipe(t_an + p_an + n_an + i_an + o_an + s_an + I_an ~ ., data = dat)
# 
# # 10 fold cross validation
# folds <- vfold_cv(dat, repeats = 10)
# folds <- folds %>%
#   mutate(recipes = map(splits, prepper, recipe = rec))
# 
# 
# library(pls)
# 
# get_var_explained <- function(recipe, ...) {
#   
#   # Extract the predictors and outcomes into their own matrices
#   y_mat <- bake(recipe, new_data = NULL, composition = "matrix", all_outcomes())
#   x_mat <- bake(recipe, new_data = NULL, composition = "matrix", all_predictors())
#   
#   # The pls package prefers the data in a data frame where the outcome
#   # and predictors are in _matrices_. To make sure this is formatted
#   # properly, use the `I()` function to inhibit `data.frame()` from making
#   # all the individual columns. `pls_format` should have two columns.
#   pls_format <- data.frame(
#     endpoints = I(y_mat),
#     measurements = I(x_mat)
#   )
#   # Fit the model
#   mod <- plsr(endpoints ~ measurements, data = pls_format)
#   
# 
#   
#   # Get the proportion of the predictor variance that is explained
#   # by the model for different number of components.
#   # xve <- explvar(mod)/100
#   # 
#   # # To do the same for the outcome, it is more complex. This code
#   # # was extracted from pls:::summary.mvr.
#   # explained <-
#   #   drop(pls::R2(mod, estimate = "train", intercept = FALSE)$val) %>%
#   #   # transpose so that components are in rows
#   #   t() %>%
#   #   as_tibble() %>%
#   #   # Add the predictor proportions
#   #   mutate(predictors = cumsum(xve) %>% as.vector(),
#   #          components = seq_along(xve)) %>%
#   #   # Put into a tidy format that is tall
#   #   pivot_longer(
#   #     cols = c(-components),
#   #     names_to = "source",
#   #     values_to = "proportion"
#   #   )
# }
# 
# 
# folds <- 
#   folds %>%
#   mutate(var = map(recipes, get_var_explained),
#          var = unname(var))
# 
# variance_data <- 
#   bind_rows(folds[["var"]]) %>%
#   filter(components <= 15) %>%
#   group_by(components, source) %>%
#   summarize(proportion = mean(proportion))
# 
# 
# ggplot(variance_data, aes(x = components, y = proportion, col = source)) + 
#   geom_line(alpha = 0.5, size = 1.2) + 
#   geom_point()