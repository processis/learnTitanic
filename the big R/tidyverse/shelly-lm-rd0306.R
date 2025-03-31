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

# 1. 读取数据
#data <- read_csv("/media/user/娱乐/learnTitanic/the big R/tidyverse/cleanDesharnais77.csv")

data <- read_csv("/media/user/娱乐/learnTitanic/the big R/tidyverse/desharnais77.csv")


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
         
         # 4. 创建线性回归模型
         lm_model <- linear_reg() %>%
           set_engine("lm") %>%
           set_mode("regression")
         
         # 5. 创建随机森林模型
         rf_model <- rand_forest() %>%
           set_engine("ranger") %>%
           set_mode("regression")
         
         
         #创建svm模型
         svm_model <- svm_rbf(cost = 1, rbf_sigma = 0.1) %>%
           set_engine("kernlab") %>%
           set_mode("regression")
         
         
         #创建nnet模型
         
         nnet_model <- mlp(hidden_units = 5, epochs = 100) %>%
           set_engine("nnet") %>%
           set_mode("regression")
         
         
         ridge_model <- linear_reg(penalty = 0.1, mixture = 0) %>%  # mixture = 0 表示纯岭回归
           set_engine("glmnet") %>%
           set_mode("regression")
         
         enet_model <- linear_reg(penalty = 0.1, mixture = 0.5) %>%  # mixture = 0.5 表示弹性网络
           set_engine("glmnet") %>%
           set_mode("regression")
         
         
    #     pls_model <- pls(num_comp = 5) %>%  # 偏最小二乘法
     #      set_engine("pls") %>%
    #       set_mode("regression")
         
    #     pcr_workflow <- workflow() %>%
    #       add_recipe(recipe) %>%
    #       add_model(pcr_model)
         
         
         
   #      mars_model <- mars(num_terms = 10) %>%  # 多元自适应回归样条
    #       set_engine("earth") %>%
   #        set_mode("regression")
         
   #      cubist_model <- cubist_rules(committees = 10) %>%  # Cubist
   #        set_engine("Cubist") %>%
  #         set_mode("regression")
         
         
         
         
         # 6. 创建配方（recipe）
         # 假设目标变量是 `Effort`，其他变量是预测变量
        # recipe <- recipe(Effort ~ ., data = train_data)
         
         #####
         
         recipe <- recipe(Effort ~ PointsNonAdjust + TeamExp, data = train_data)
         
       
         
         # 7. 创建工作流
         lm_workflow <- workflow() %>%
           add_recipe(recipe) %>%
           add_model(lm_model)
         
         
         rf_workflow <- workflow() %>%
           add_recipe(recipe) %>%
           add_model(rf_model)
         
       
         ##
         
         svm_workflow <- workflow() %>%
           add_recipe(recipe) %>%
           add_model(svm_model)
         
         nnet_workflow <- workflow() %>%
           add_recipe(recipe) %>%
           add_model(nnet_model)
         
         
         ridge_workflow <- workflow() %>%
           add_recipe(recipe) %>%
           add_model(ridge_model)
         
         enet_workflow <- workflow() %>%
           add_recipe(recipe) %>%
           add_model(enet_model)
         
         
         
   #      pls_workflow <- workflow() %>%
   #        add_recipe(recipe) %>%
   #        add_model(pls_model)
         
    #     pcr_workflow <- workflow() %>%
    #       add_recipe(recipe) %>%
    #       add_model(pcr_model)
         
     #    mars_workflow <- workflow() %>%
    #       add_recipe(recipe) %>%
    #       add_model(mars_model)
         
      #   cubist_workflow <- workflow() %>%
     #      add_recipe(recipe) %>%
     #      add_model(cubist_model)
         
         

         
         # 8. 拟合模型
         lm_fit <- fit(lm_workflow, data = train_data)
         rf_fit <- fit(rf_workflow, data = train_data)
         
         svm_fit <- fit(svm_workflow, data = train_data)
         nnet_fit <- fit(nnet_workflow, data = train_data)
         
         ridge_fit <- fit(ridge_workflow, data = train_data)
         enet_fit <- fit(enet_workflow, data = train_data)
         
         
    #     pls_fit <- fit(pls_workflow, data = train_data)
   #      pcr_fit <- fit(pcr_workflow, data = train_data)
   #      mars_fit <- fit(mars_workflow, data = train_data)
   #      cubist_fit <- fit(cubist_workflow, data = train_data)
         
         # 9. 预测
         lm_predictions <- predict(lm_fit, new_data = test_data)
         rf_predictions <- predict(rf_fit, new_data = test_data)
         
         svm_predictions <- predict(svm_fit, new_data = test_data)
         nnet_predictions <- predict(nnet_fit, new_data = test_data)
         
         ridge_predictions <- predict(ridge_fit, new_data = test_data)
         enet_predictions <- predict(enet_fit, new_data = test_data)
         
         # 10. 评估模型
         lm_metrics <- lm_predictions %>%
           bind_cols(test_data) %>%
           metrics(truth = Effort, estimate = .pred)
         
         rf_metrics <- rf_predictions %>%
           bind_cols(test_data) %>%
           metrics(truth = Effort, estimate = .pred)
         
         
         svm_metrics <- svm_predictions %>%
           bind_cols(test_data) %>%
           metrics(truth = Effort, estimate = .pred)
         
         nnet_metrics <- nnet_predictions %>%
           bind_cols(test_data) %>%
           metrics(truth = Effort, estimate = .pred)
         
         
         ridge_metrics <- ridge_predictions %>%
           bind_cols(test_data) %>%
           metrics(truth = Effort, estimate = .pred)
         
         enet_metrics <- enet_predictions %>%
           bind_cols(test_data) %>%
           metrics(truth = Effort, estimate = .pred)
         
         
   #      pls_metrics <- pls_predictions %>%
   #        bind_cols(test_data) %>%
   #        metrics(truth = Effort, estimate = .pred)
         
    #     pcr_metrics <- pcr_predictions %>%
    #       bind_cols(test_data) %>%
   #        metrics(truth = Effort, estimate = .pred)
         
   #      mars_metrics <- mars_predictions %>%
   #        bind_cols(test_data) %>%
   #        metrics(truth = Effort, estimate = .pred)
         
   #      cubist_metrics <- cubist_predictions %>%
   #        bind_cols(test_data) %>%
   #        metrics(truth = Effort, estimate = .pred)
         
         
         
         
         # 11. 提取 RMSE 和 R-squared
         lm_rmse <- lm_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
         lm_rsq <- lm_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         rf_rmse <- rf_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
         rf_rsq <- rf_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         svm_rmse <- svm_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
         svm_rsq <- svm_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         nnet_rmse <- nnet_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
         nnet_rsq <- nnet_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         ridge_rmse <- ridge_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
         ridge_rsq <- ridge_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         enet_rmse <- enet_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
         enet_rsq <- enet_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         
   #      pls_rmse <- pls_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
   #      pls_rsq <- pls_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
   #      pcr_rmse <- pcr_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
   #      pcr_rsq <- pcr_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
   #      mars_rmse <- mars_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
   #      mars_rsq <- mars_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
    #     cubist_rmse <- cubist_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
    #     cubist_rsq <- cubist_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
         
         
         
         
         
         
         # 12. 保存结果到数据表
         results <- tibble(
           Model = c("Linear Regression", "Random Forest","SVM", "Neural Network","Ridge Regression", "Elastic Net"),
           RMSE = c(lm_rmse, rf_rmse,svm_rmse, nnet_rmse,ridge_rmse, enet_rmse),
           Rsquared = c(lm_rsq, rf_rsq,svm_rsq, nnet_rsq,ridge_rsq, enet_rsq)
         )
         
         
         
         
         # 13. 保存结果到 CSV 文件
         write_csv(results, "/home/user/Downloads/model_results.csv")
         
         # 打印结果
         print(results)