# 加载必要的库
library(tidymodels)
library(readr)
library(dplyr)
library(ranger)

# 1. 读取数据
data <- read_csv("/media/user/娱乐/learnTitanic/the big R/tidyverse/cleanDesharnais77.csv")

# 2. 处理空值
# 假设我们用均值填充数值列中的空值 not 
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
         
         # 4. 创建线性回归模型
         lm_model <- linear_reg() %>%
           set_engine("lm") %>%
           set_mode("regression")
         
         # 5. 创建随机森林模型
         rf_model <- rand_forest() %>%
           set_engine("ranger") %>%
           set_mode("regression")
         
         # 6. 创建配方（recipe）
         # 假设目标变量是 `Effort`，其他变量是预测变量
         recipe <- recipe(Effort ~ ., data = train_data)
         
         #####
         
         recipe <- recipe(Effort ~ PointsNonAdjust + TeamExp, data = train_data)
         
       
         
         # 7. 创建工作流
         lm_workflow <- workflow() %>%
           add_recipe(recipe) %>%
           add_model(lm_model)
         
         
         rf_workflow <- workflow() %>%
           add_recipe(recipe) %>%
           add_model(rf_model)
         
       
         

         
         # 8. 拟合模型
         lm_fit <- fit(lm_workflow, data = train_data)
         rf_fit <- fit(rf_workflow, data = train_data)
         
         
         
         # 9. 预测
         lm_predictions <- predict(lm_fit, new_data = test_data)
         rf_predictions <- predict(rf_fit, new_data = test_data)
         
         # 10. 评估模型
         lm_metrics <- lm_predictions %>%
           bind_cols(test_data) %>%
           metrics(truth = Effort, estimate = .pred)
         
         rf_metrics <- rf_predictions %>%
           bind_cols(test_data) %>%
           metrics(truth = Effort, estimate = .pred)
         
         # 11. 提取 RMSE 和 R-squared
         lm_rmse <- lm_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
         lm_rsq <- lm_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         rf_rmse <- rf_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
         rf_rsq <- rf_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         # 12. 保存结果到数据表
         results <- tibble(
           Model = c("Linear Regression", "Random Forest"),
           RMSE = c(lm_rmse, rf_rmse),
           Rsquared = c(lm_rsq, rf_rsq)
         )
         
         # 13. 保存结果到 CSV 文件
         write_csv(results, "model_results.csv")
         
         # 打印结果
         print(results)