library(tidymodels)
library(vip)
data(cells)
cell_split <- initial_split(cells %>% select(-case), strata = class)
cell_train <- training(cell_split)
cell_test  <- testing(cell_split)

tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

tree_grid

set.seed(234)
cell_folds <- vfold_cv(cell_train)

set.seed(345)

tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(class ~ .)

tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = cell_folds,
    grid = tree_grid
  )


best_tree <- tree_res %>%
  select_best("accuracy")

best_tree

final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)


final_fit <- 
  final_wf %>%
  last_fit(cell_split) 

final_fit %>%
  collect_metrics()

final_fit %>%
  collect_predictions() %>% 
  roc_curve(class, .pred_PS) %>% 
  autoplot()

final_tree <- extract_workflow(final_fit)

final_tree %>% 
  extract_fit_parsnip() %>% 
  vip()