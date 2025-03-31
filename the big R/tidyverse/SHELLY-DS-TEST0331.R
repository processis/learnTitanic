# Load required packages
library(tidymodels)  # Includes parsnip, recipes, etc.
library(Cubist)      # Engine for cubist models
library(mlbench)     # For the BostonHousing dataset

# Load example data
data(BostonHousing)
boston <- BostonHousing %>% 
  select(-chas)  # Remove categorical variable for simplicity

# Split data into training and testing sets  将数据分成训练集和测试集
set.seed(123)
split <- initial_split(boston, prop = 0.75)
train_data <- training(split)
test_data <- testing(split)

# Create a recipe for preprocessing 创建一个预处理配方
boston_recipe <- recipe(medv ~ ., data = train_data) %>%
  step_normalize(all_numeric_predictors())  # Standardize numeric predictors

# Define the Cubist model specification 定义立体主义模型规范
cubist_spec <- cubist_rules(
  committees = tune(),      # Number of committees (models)
  neighbors = tune()        # Number of neighbors for adjustment (0-9)
) %>% 
  set_engine("Cubist") %>% 
  set_mode("regression")

# Set up a workflow
cubist_wf <- workflow() %>%
  add_recipe(boston_recipe) %>%
  add_model(cubist_spec)

# Create cross-validation folds 创建交叉验证折叠
set.seed(456)
folds <- vfold_cv(train_data, v = 5)

# Define tuning grid 定义调谐网格
tune_grid <- grid_regular(
  committees(range = c(1, 10)),
  neighbors(range = c(0, 9)),
  levels = 5
)

# Tune the model  调整模型
tune_results <- tune_grid(
  cubist_wf,
  resamples = folds,
  grid = tune_grid,
  metrics = metric_set(rmse, rsq)
)

# Select the best model based on RMSE 根据RMSE选择最佳模型
best_params <- select_best(tune_results, metric = "rmse")

# Finalize the workflow with the best parameters 用最佳参数确定工作流程
final_wf <- finalize_workflow(cubist_wf, best_params)

# Train the final model on the full training data 在完整的训练数据上训练最终模型
final_model <- fit(final_wf, data = train_data)

# Evaluate on test data
test_results <- test_data %>%
  bind_cols(predict(final_model, new_data = test_data)) %>%
  metrics(truth = medv, estimate = .pred)

# View test metrics
test_results

# Examine the final model details 检查最终的模型细节
extract_fit_engine(final_model)

# Variable importance 变量的重要性
library(vip)
final_model %>%
  extract_fit_parsnip() %>%
  vip(method = "model")