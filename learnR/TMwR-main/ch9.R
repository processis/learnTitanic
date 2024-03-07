library(tidymodels)
library(kableExtra)
tidymodels_prefer()
source("ames_snippets.R")
load("RData/lm_fit.RData")

data(ad_data)
set.seed(245)
ad_folds <- vfold_cv(ad_data, repeats = 5)

set.seed(234)
n <- 200
obs <- runif(n, min = 2, max = 20)

reg_ex <- 
  tibble(
    observed = c(obs, obs),
    predicted = c(obs + rnorm(n, sd = 1.5), 5 + .5 * obs + rnorm(n, sd = .5)),
    approach = rep(c("RMSE optimized", "R^2 optimized"), each = n)
  ) %>% 
  mutate(approach = factor(
    approach, 
    levels = c("RMSE optimized", "R^2 optimized"),
    labels = c(expression(RMSE ~ optimized), expression(italic(R^2) ~ optimized)))
  )

ggplot(reg_ex, aes(x = observed, y = predicted)) + 
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  coord_obs_pred() + 
  facet_wrap(~ approach, labeller = "label_parsed")

ad_mod <- logistic_reg() %>% set_engine("glm") 
full_model_fit <-
  ad_mod %>% 
  fit(Class ~ (Genotype + male + age)^3, data = ad_data)

full_model_fit %>% extract_fit_engine() 

two_way_fit <-
  ad_mod %>% 
  fit(Class ~ (Genotype + male + age)^2, data = ad_data)

three_factor_test <- 
  anova(
    full_model_fit %>% extract_fit_engine(), 
    two_way_fit %>% extract_fit_engine(),
    test = "LRT"
  )

main_effects_fit <-
  ad_mod %>% 
  fit(Class ~ Genotype + male + age, data = ad_data)

two_factor_test <- 
  anova(
    two_way_fit %>% extract_fit_engine(), 
    main_effects_fit %>% extract_fit_engine(),
    test = "LRT"
  )

two_factor_rs <- 
  ad_mod %>% 
  fit_resamples(Class ~ (Genotype + male + age)^2, ad_folds)

two_factor_res <- 
  collect_metrics(two_factor_rs) %>% 
  filter(.metric == "accuracy") %>% 
  pull(mean)


ames_test_res <- predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
ames_test_res
ames_test_res <- bind_cols(ames_test_res, ames_test %>% select(Sale_Price))
ames_test_res
ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) + 
  # Create a diagonal line:
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  # Scale and size the x- and y-axis uniformly:
  coord_obs_pred()

rmse(ames_test_res, truth = Sale_Price, estimate = .pred)
ames_metrics <- metric_set(rmse, rsq, mae)
ames_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)

data(two_class_example)
tibble(two_class_example)
conf_mat(two_class_example, truth = truth, estimate = predicted)

# Accuracy:
accuracy(two_class_example, truth, predicted)

# Matthews correlation coefficient:
mcc(two_class_example, truth, predicted)

# F1 metric:
f_meas(two_class_example, truth, predicted)

# Combining these three classification metrics together
classification_metrics <- metric_set(accuracy, mcc, f_meas)
classification_metrics(two_class_example, truth = truth, estimate = predicted)

f_meas(two_class_example, truth, predicted, event_level = "second")

two_class_curve <- roc_curve(two_class_example, truth, Class1)
two_class_curve

roc_auc(two_class_example, truth, Class1)

autoplot(two_class_curve)

data(hpc_cv)
tibble(hpc_cv)
accuracy(hpc_cv, obs, pred)

mcc(hpc_cv, obs, pred)

class_totals <- 
  count(hpc_cv, obs, name = "totals") %>% 
  mutate(class_wts = totals / sum(totals))
class_totals

cell_counts <- 
  hpc_cv %>% 
  group_by(obs, pred) %>% 
  count() %>% 
  ungroup()

# Compute the four sensitivities using 1-vs-all
one_versus_all <- 
  cell_counts %>% 
  filter(obs == pred) %>% 
  full_join(class_totals, by = "obs") %>% 
  mutate(sens = n / totals)
one_versus_all

# Three different estimates:
one_versus_all %>% 
  summarize(
    macro = mean(sens), 
    macro_wts = weighted.mean(sens, class_wts),
    micro = sum(n) / sum(totals)
  )

sensitivity(hpc_cv, obs, pred, estimator = "macro")
sensitivity(hpc_cv, obs, pred, estimator = "macro_weighted")
sensitivity(hpc_cv, obs, pred, estimator = "micro")

roc_auc(hpc_cv, obs, VF, F, M, L)

roc_auc(hpc_cv, obs, VF, F, M, L, estimator = "macro_weighted")

hpc_cv %>% 
  group_by(Resample) %>% 
  accuracy(obs, pred)
hpc_cv %>% 
  group_by(Resample) %>% 
  roc_curve(obs, VF, F, M, L) %>% 
  autoplot()

