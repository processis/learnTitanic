install.packages("tidymodels")
install.packages("ranger")

library(tidymodels)
library(ranger)

data(mtcars)
mtcars <- mtcars %>% 
  mutate(across(where(is.character), as.factor))

# Split the data into training and testing sets
set.seed(123)
split <- initial_split(mtcars, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)

rf_model <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_recipe <- recipe(mpg ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors()))

rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_model)

set.seed(123)
folds <- vfold_cv(train_data, v = 5)

rf_tune <- tune_grid(
  rf_workflow,
  resamples = folds,
  grid = 10,
  control = control_grid(verbose = TRUE)
)

# Select the best hyperparameters
best_params <- select_best(rf_tune, metric = "rmse")

final_rf_fit <- final_rf_workflow %>%
  fit(data = train_data)

predictions <- predict(final_rf_fit, new_data = test_data) %>%
  bind_cols(test_data)

# Evaluate the model
metrics <- metric_set(rmse, rsq)
metrics(predictions, truth = mpg, estimate = .pred)

library(ggplot2)

ggplot(predictions, aes(x = mpg, y = .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual MPG", x = "Actual MPG", y = "Predicted MPG")


