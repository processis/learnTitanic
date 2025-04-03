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



# 4. 创建svm模型
svm_model <- svm_rbf(cost = 1, rbf_sigma = 0.1) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# 7. 创建工作流
svm_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(svm_model)

# 8. 拟合模型
svm_fit <- fit(svm_workflow, data = train_data)

# 9. 预测
svm_predictions <- predict(svm_fit, new_data = test_data)


# 10. 评估模型
svm_metrics <- svm_predictions %>%
  bind_cols(test_data) %>%
  metrics(truth = Effort, estimate = .pred)

# 11. 提取 RMSE 和 R-squared
svm_rmse <- svm_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
svm_rsq <- svm_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)

# 打印结果
print(results)