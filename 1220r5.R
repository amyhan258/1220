library(tidymodels)
library(readr) 
library(vip)

hotels <- 
  read_csv("https://tidymodels.org/start/case-study/hotels.csv") %>%
  mutate(across(where(is.character), as.factor))

hotels %>% 
  count(children) %>% 
  mutate(prop = n/sum(n))

set.seed(123)
splits      <- initial_split(hotels, strata = children)

hotel_other <- training(splits)
hotel_test  <- testing(splits)

set.seed(234)
val_set <- validation_split(hotel_other, 
                            strata = children, 
                            prop = 0.80)

#1
lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date, holidays = holidays) %>% 
  step_rm(arrival_date) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())


#step_dummy() converts characters or factors (i.e., nominal variables) into one or more numeric binary model terms for the levels of the original data.
# step_zv() removes indicator variables that only contain a single unique value (e.g. all zeros).
# step_normalize() centers and scales numeric variables.



lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)


lr_reg_grid <-grid_regular(penalty(),levels = 30)



lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

# top_models <-
#   lr_res %>% 
#   show_best("roc_auc", n = 15) %>% 
#   arrange(penalty) 
# top_models

lr_best <- 
  lr_res %>% 
  select_best("roc_auc")



lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Logistic Regression")




autoplot(lr_auc)