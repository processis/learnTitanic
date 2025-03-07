# 加载必要的库
library(tidymodels)
library(readr)
library(dplyr)
library(kernlab)  # 用于支持向量机 (SVM)
library(nnet)     # 用于神经网络 (NNET)

# 1. 读取数据
data <- read_csv("your_data.csv")

# 2. 处理空值
# 假设我们用均值填充数值列中的空值
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
         
         # 3. 划分训练集和测试集
         set.seed(123)
         data_split <- initial_split(data, prop = 0.75)
         train_data <- training(data_split)
         test_data <- testing(data_split)
         
         # 4. 创建配方（recipe）
         # 假设目标变量是 `target`，其他变量是预测变量
         recipe <- recipe(target ~ ., data = train_data)
         
         # 5. 定义模型
         svm_model <- svm_rbf(cost = 1, rbf_sigma = 0.1) %>%
           set_engine("kernlab") %>%
           set_mode("regression")
         
         nnet_model <- mlp(hidden_units = 5, epochs = 100) %>%
           set_engine("nnet") %>%
           set_mode("regression")
         
         # 6. 创建工作流
         svm_workflow <- workflow() %>%
           add_recipe(recipe) %>%
           add_model(svm_model)
         
         nnet_workflow <- workflow() %>%
           add_recipe(recipe) %>%
           add_model(nnet_model)
         
         # 7. 拟合模型
         svm_fit <- fit(svm_workflow, data = train_data)
         nnet_fit <- fit(nnet_workflow, data = train_data)
         
         # 8. 预测
         svm_predictions <- predict(svm_fit, new_data = test_data)
         nnet_predictions <- predict(nnet_fit, new_data = test_data)
         
         # 9. 评估模型
         svm_metrics <- svm_predictions %>%
           bind_cols(test_data) %>%
           metrics(truth = target, estimate = .pred)
         
         nnet_metrics <- nnet_predictions %>%
           bind_cols(test_data) %>%
           metrics(truth = target, estimate = .pred)
         
         # 10. 提取 RMSE 和 R-squared
         svm_rmse <- svm_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
         svm_rsq <- svm_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         nnet_rmse <- nnet_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
         nnet_rsq <- nnet_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         # 11. 保存结果到数据表
         results <- tibble(
           Model = c("SVM", "Neural Network"),
           RMSE = c(svm_rmse, nnet_rmse),
           Rsquared = c(svm_rsq, nnet_rsq)
         )
         
         # 12. 保存结果到 CSV 文件
         write_csv(results, "svm_nnet_results.csv")
         
         # 打印结果
         print(results)