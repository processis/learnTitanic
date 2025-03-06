# Read Desharnais77 public dataset from promise uottawa repository
#desharnais <- read.table("desharnaisLogEffort77.csv",sep = ",", header = TRUE)  #old non-tidy read
library(tidyverse)
library(tidymodels)
tidymodels_prefer()
tbDesharnais <- read_csv("/media/user/娱乐/learnTitanic/the big R/tidyverse/cleanDesharnais77.csv",col_names = TRUE,na = '-1',
  cols(
    'Project' = col_integer(),
    'TeamExp' = col_integer(),
    'ManagerExp' = col_integer(),
    'YearEnd' = col_integer(),
    'Length' = col_integer(),
    'Effort' = col_integer(),
    'Transactions' = col_integer(),
    'Entities' = col_integer(),
    'PointsNonAdjust' = col_integer(),
    'Adjustment' = col_integer(),
    'PointsAjust' = col_integer(),
    'Language' = col_character(),
  )
)
#spec(tbDesharnais)  #error
print(tbDesharnais)
#histograms , check skew
ggplot(tbDesharnais, aes(x = Effort)) + geom_histogram(bins = 10, col="white")
#histograms , check skew
ggplot(tbDesharnais, aes(x = PointsNonAdjust)) + geom_histogram(bins = 10, col="white")
#histograms , check skew
ggplot(tbDesharnais, aes(x = PointsAjust)) + geom_histogram(bins = 10, col="white")
# mutate all these 3 skewed variables by Log
tbDesharnais <- tbDesharnais %>% mutate(Effort = log10(Effort))
tbDesharnais <- tbDesharnais %>% mutate(PointsAjust = log10(PointsAjust))
tbDesharnais <- tbDesharnais %>% mutate(PointsNonAdjust = log10(PointsNonAdjust))
#check more variables , if skew
#tbDesharnais %>% replace_na(list(TeamExp = 2 , ManagerExp =3))
#tbDesharnais$TeamExp %>% replace_na(2)
#tbDesharnais %>% dplyr::mutate(TeamExp = replace_na(TeamExp,2))
#tbDesharnais %>% fill(TeamExp,ManagerExp)
ggplot(tbDesharnais, aes(x = TeamExp)) + geom_histogram(bins = 5, col="white")
#tbDesharnais %>% replace_na(ManagerExp = 3)
#tbDesharnais$ManagerExp %>% replace_na(3)
ggplot(tbDesharnais, aes(x = ManagerExp)) + geom_histogram(bins = 5, col="white")
#use random number to split training vs testing
set.seed(502)
tbDesharnais_split <- initial_split(tbDesharnais, prop = 0.85) # strata = Effort , not use
tbDesharnais_train <- training(tbDesharnais_split)
tbDesharnais_test  <-  testing(tbDesharnais_split)
#TidyModel Ch6 fit models with parsnip
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(Effort ~ PointsNonAdjust + TeamExp, data = tbDesharnais_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = tbDesharnais_train %>% select(PointsNonAdjust, TeamExp),
    y = tbDesharnais_train %>% pull(Effort)
  )

lm_form_fit
lm_xy_fit
#extract fit
lm_form_fit %>% extract_fit_engine()
#Normal methods can be applied to this object, such as printing and plotting:
lm_form_fit %>% extract_fit_engine() %>% vcov()
#
model_res <- 
  lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)
param_est
#
tidy(lm_form_fit)
#make predictions, numeric data
tbDesharnais_test_small <- tbDesharnais_test %>% slice(1:7)
predict(lm_form_fit, new_data = tbDesharnais_test_small)
#merge with original data
tbDesharnais_test_small %>% 
  select(Effort) %>% 
  bind_cols(predict(lm_form_fit, tbDesharnais_test_small)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, tbDesharnais_test_small, type = "pred_int")) 
#same format for tree model, output in same format
tree_model <- 
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit <- 
  tree_model %>% 
  fit(Effort ~ PointsNonAdjust + TeamExp, data = tbDesharnais_train)

tbDesharnais_test_small %>% 
  select(Effort) %>% 
  bind_cols(predict(tree_fit, tbDesharnais_test_small))
#6 Fitting Models with parnip    www.tmwr.org/models
linear_reg() %>% set_engine("lm")
linear_reg() %>% set_engine("glmnet")
linear_reg() %>% set_engine("stan")
# translate provide details
linear_reg() %>% set_engine("lm") %>% translate()
linear_reg(penalty = 1) %>% set_engine("glmnet") %>% translate()
linear_reg() %>% set_engine("stan") %>% translate()
#follow ames e.g. for desharnais
lm_model <- linear_reg() %>% set_engine("lm")
lm_form_fit <- lm_model %>% fit(Effort ~ TeamExp + PointsNonAdjust, data = tbDesharnais_train)
lm_xy_fit <- lm_model %>%
  fit_xy(
    x = tbDesharnais_train %>% select(PointsNonAdjust,TeamExp),
    y = tbDesharnais_train %>% pull(Effort)
  )
lm_form_fit
lm_xy_fit
#random forest model functions as example
rand_forest(trees = 1000, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("regression") %>%
  translate()
#main arguments are commonly used across engines; engine arguments are specific
rand_forest(trees = 1000, min_n = 5) %>%
  set_engine("ranger", verbose = TRUE) %>%
  set_mode("regression") %>%
  translate()
#
lm_model <-rand_forest(trees = 1000, min_n = 5)  %>% set_engine("ranger")
lm_form_fit <- lm_model %>% fit(Effort ~ TeamExp + PointsNonAdjust, data = tbDesharnais_train)
# rstudio.github.io/rstudioaddins
# parsnip_addin()
#CH6 workflow codes
library(tidymodels)
library(workflowsets)
#library(kableExtra)
#library(censored)
#library(survival)
library(parsnip)
tidymodels_prefer()

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)

lm_wflow

#simple  2 predictors lm
lm_wflow <- 
  lm_wflow %>% 
  add_formula(Effort ~ PointsNonAdjust + TeamExp)

lm_wflow
#workflows fit() method to create the model
lm_fit <- fit(lm_wflow, tbDesharnais_train)
lm_fit
#We can also `predict()` on the fitted workflow:
predict(lm_fit, tbDesharnais_test %>% slice(1:7))
# get the same 7 predicted values
