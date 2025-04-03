# 加载必要的库
library(tidymodels)
library(readr)
library(dplyr)
library(ranger)
library(kernlab)  # 用于支持向量机 (SVM)
library(nnet)     # 用于神经网络 (NNET)
library(glmnet)  # 用于岭回归和弹性网络
library(pls)     # 用于 PLS 和 PCR
library(earth)   # 用于 MARS
library(Cubist)  # 用于 Cubist
library(rules)   # 用于 Cubist
library(kknn)

# 1. 读取数据
#data <- read_csv("/media/user/娱乐/learnTitanic/the big R/tidyverse/cleanDesharnais77.csv")

data <- read_csv("/media/user/娱乐/SHELLY-SD-0331TEST/desharnais77.csv")


#test <- read_csv("/media/user/娱乐/learnTitanic/the big R/tidyverse/test.csv")

#NA replace -1

data<-data%>%
  mutate(across(everything(),~ifelse(.<0,NA,.)))

# 使用 mutate 和 across 函数来替换 -1 值
#data <- data %>%
#  mutate(across(everything(), ~ ifelse(. == -1, cur_column() %>% str_remove("col") %>% as.numeric() + 1, .)))


# 2. 处理空值
# 假设我们用均值填充数值列中的空值 not 
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))


#test <- test %>%
#  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))


#data$column[is.na(data$column)]<-mean(data$column,na.rm=TRUE)


##### mutate all these 3 skewed variables by Log
data <- data %>% mutate(Effort = log10(Effort))
data <- data %>% mutate(PointsAjust = log10(PointsAjust))
data <- data %>% mutate(PointsNonAdjust = log10(PointsNonAdjust))

# 3. 划分训练集和测试集
set.seed(123)
data_split <- initial_split(data, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)


recipe <- recipe(Effort ~ PointsNonAdjust + TeamExp, data = train_data)



#KKNN

# 注意：KKNN需要标准化（因为基于距离）
recipe_spec <- recipe(
  Effort ~ PointsNonAdjust + TeamExp,  # 假设目标列名为"target"
  data = train_data
) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())  # 分类变量转为虚拟变量

## 5. 定义KKNN模型 ----
knn_model <- nearest_neighbor(
  mode = "regression",
  neighbors = tune(),
  weight_func = tune(),
  dist_power = tune()
) %>%
  set_engine("kknn")

## 6. 创建工作流 ----
knn_wf <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(knn_model)

## 7. 交叉验证调参 ----
set.seed(456)
folds <- vfold_cv(train_data, v = 5)

knn_res <- tune_grid(
  knn_wf,
  resamples = folds,
  grid = 10,  # 自动生成10种参数组合
  metrics = metric_set(rmse, rsq)
)

## 8. 选择最佳模型 ----
best_knn <- select_best(knn_res, metric = "rmse")
final_wf <- finalize_workflow(knn_wf, best_knn)

## 9. 最终评估 ----
final_fit <- last_fit(final_wf, split)
final_metrics <- collect_metrics(final_fit)

KKNN_rmse<-final_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
KKNN_rsq<-final_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)


# 打印结果
print(results)