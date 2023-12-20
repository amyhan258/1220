library(tidymodels)

rand_forest(trees = 2000, min_n = 10) %>%                   # <- main arguments
  set_engine("ranger", regularization.factor = 0.5)         # <- engine-specific
neural_net_spec <- 
  mlp(hidden_units = tune()) %>%
  set_mode("regression") %>%
  set_engine("keras")

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train)  %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = tune()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Longitude, deg_free = tune("longitude df")) %>% 
  step_ns(Latitude,  deg_free = tune("latitude df"))

recipes_param <- extract_parameter_set_dials(ames_rec)
recipes_param

wflow_param <- 
  workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(neural_net_spec) %>% 
  extract_parameter_set_dials()
wflow_param

wflow_param %>% extract_parameter_dials("threshold")

extract_parameter_set_dials(ames_rec) %>% 
  update(threshold = threshold(c(0.8, 1.0)))


# rf?
rf_spec <- 
  rand_forest(mtry = tune()) %>% 
  set_engine("ranger", regularization.factor = tune("regularization")) %>%
  set_mode("regression")

rf_param <- extract_parameter_set_dials(rf_spec)

rf_param %>% 
  update(mtry = mtry(c(1, 70)))