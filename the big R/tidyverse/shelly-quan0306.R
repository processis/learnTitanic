# 加载必要的库
library(tidymodels)
library(readr)
library(dplyr)
library(ranger)
library(glmnet)  # 用于岭回归和弹性网络
library(pls)     # 用于偏最小二乘法和主成分回归
library(nnet)    # 用于神经网络
library(earth)   # 用于多元自适应回归样条 (MARS)
library(kernlab) # 用于支持向量机 (SVM)
library(Cubist)  # 用于 Cubist
library(rules)   # 用于 Cubist

# 1. 读取数据
data <- read_csv("/media/user/娱乐/learnTitanic/the big R/tidyverse/cleanDesharnais77.csv")

# 2. 处理空值
# 假设我们用均值填充数值列中的空值
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

##### mutate all these 3 skewed variables by Log
data <- data %>% mutate(Effort = log10(Effort))
data <- data %>% mutate(PointsAjust = log10(PointsAjust))
data <- data %>% mutate(PointsNonAdjust = log10(PointsNonAdjust))

# 3. 划分训练集和测试集
set.seed(123)
data_split <- initial_split(data, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

# 4. 创建配方（recipe）
# 假设目标变量是 `target`，其他变量是预测变量
#recipe <- recipe(target ~ ., data = train_data)

#####

recipe <- recipe(Effort ~ PointsNonAdjust + TeamExp, data = train_data)


# 5. 定义模型
models <- list(
  linear_reg = linear_reg() %>% set_engine("lm") %>% set_mode("regression"),
  ridge = linear_reg(penalty = 0.1, mixture = 0) %>% set_engine("glmnet") %>% set_mode("regression"),
  enet = linear_reg(penalty = 0.1, mixture = 0.5) %>% set_engine("glmnet") %>% set_mode("regression"),
  pls = pls(num_comp = 5) %>% set_engine("pls") %>% set_mode("regression"),
  pcr = pls(num_comp = 5) %>% set_engine("pls") %>% set_mode("regression"),
  nnet = mlp(hidden_units = 5) %>% set_engine("nnet") %>% set_mode("regression"),
  mars = mars(num_terms = 10) %>% set_engine("earth") %>% set_mode("regression"),
  svm = svm_rbf(cost = 1, rbf_sigma = 0.1) %>% set_engine("kernlab") %>% set_mode("regression"),
  cubist = cubist_rules(committees = 10) %>% set_engine("Cubist") %>% set_mode("regression"),
  rf = rand_forest(trees = 100) %>% set_engine("ranger") %>% set_mode("regression")
)

# 6. 创建工作流并拟合模型
results <- tibble(model = character(), rmse = numeric(), rsq = numeric())

for (model_name in names(models)) {
  workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(models[[model_name]])
  
  fit <- fit(workflow, data = train_data)
  predictions <- predict(fit, new_data = test_data)
  
  metrics <- predictions %>%
    bind_cols(test_data) %>%
    metrics(truth = Effort, estimate = .pred)
  
  rmse <- metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
  rsq <- metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
  
  results <- results %>%
    add_row(model = model_name, rmse = rmse, rsq = rsq)
}

# 7. 保存结果到 CSV 文件
write_csv(results, "model_results.csv")

# 打印结果
print(results)