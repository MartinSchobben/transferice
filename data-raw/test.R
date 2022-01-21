library(pls)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gganimate)
library(glue)




use_data(complete_data)

# convert to normal tibble
# coords <- params %>% 
#   mutate(
#     longitude = sf::st_coordinates(geometry)[1],
#     latitude = sf::st_coordinates(geometry)[2]
#   ) %>% 
#   as_tibble() 

# long format environmental params
tb_lng <- pivot_longer(tb, ends_with("_an"), names_to = "param", values_to = "value") %>% 
  # long format species list
  pivot_longer(
    -c(station, geometry, depth, weight, lycopodium, lyc_tab, param, value), 
    names_to = "species", 
    values_to = "count"
    ) %>% tidyr::drop_na(value, count)

t_an <- ggplot(filter(tb_lng, param == "t_an") , aes(x = count, y = value)) +
  geom_point() +
  #facet_grid(vars(param), vars(species), scales = "free")
  transition_states(species) +
  ggtitle('{closest_state}')


animate(
  t_an,
  fps = 1,
  width = 900,
  units = "px",
  res = 230,
  bg = 'transparent',
  renderer = gifski_renderer()
)

gganimate::anim_save( "t.gif")

# only model vars
tb <- select(as_tibble(complete_data), -c(station, depth, weight, lycopodium, lyc_tab, geometry))

# remove variable with only NAs
tb <- select(tb, -where(~all(is.na(.x))))

# select those with a minimum of 15 observations
slt <- colnames(tb)[map_dbl(tb, ~sum(!is.na(.))) > 15]
tb <- select(tb, any_of(slt)) %>% mutate(across(any_of(slt), ~tidyr::replace_na(.x, 0)))

# perform multiple multivaraite
tb_t_an <- select(tb, -c(p_an,  n_an,  i_an,  o_an,  s_an,  I_an))
lm1 <- lm(t_an ~ .,  data = tb_t_an) 

# range species count
ran <- tb_t_an %>% summarize(across(-t_an, .fns = list(min, max))) %>% range()
seqs <- seq(ran[1], ran[2], length = nrow(tb_t_an))
# diagnostics
library(ggfortify)
autoplot(lm1)

# effect size

predict(lm1, tb_t_an %>% mutate(across(-c(t_an, pgla), ~0), pgla = seqs)) %>% hist()
predict(lm1, tb_t_an %>% mutate(across(-c(t_an, imin), ~0), imin = seqs)) %>% hist()
predict(lm1, tb_t_an %>% mutate(across(-c(t_an, sant), ~0), sant = seqs)) %>% hist()



# longer
tb_ln <- pivot_longer(tb_t_an, -ends_with("_an"), names_to = "species", values_to = "count")

ggplot(tb_ln, aes(x = count, y = t_an, color = species)) +
  geom_point() +
  geom_line(data = fortify(lm1), aes(y = .fitted, color = "red"))
