library(tidymodels)
tidymodels_prefer()

data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

set.seed(1001)
ames_folds <- vfold_cv(ames_train, v = 10)
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

#######new
basic_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

interaction_rec <- 
  basic_rec %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) 

spline_rec <- 
  interaction_rec %>% 
  step_ns(Latitude, Longitude, deg_free = 50)

rf_rec <- recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
                   Latitude + Longitude, data= ames_train)
  
preproc <- 
  list(basic = basic_rec, 
       interact = interaction_rec, 
       splines = spline_rec,
       rfrf=rf_rec )


lm_models <- workflow_set(preproc, list(linear_reg(),linear_reg(),linear_reg(),rand_forest(mode="regression",engine="ranger",trees=1000)), cross = FALSE) %>% 
  workflow_map("fit_resamples", seed = 1101, verbose = TRUE, resamples = ames_folds, control = keep_pred)

lm_models

collect_metrics(lm_models) %>% 
  filter(.metric == "rmse")


library(ggrepel)
autoplot(lm_models, metric = "rsq") +
  geom_text_repel(aes(label = wflow_id), nudge_x = 1/8, nudge_y = 1/100) +
  theme(legend.position = "none")