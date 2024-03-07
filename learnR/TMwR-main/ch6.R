library(tidymodels)
library(kknn)
library(kableExtra)
library(tidyr)

tidymodels_prefer()

source("ames_snippets.R")

library(tidymodels)
tidymodels_prefer()

linear_reg() %>% set_engine("lm")

linear_reg() %>% set_engine("glmnet") 

linear_reg() %>% set_engine("stan")

linear_reg() %>% set_engine("lm") %>% translate()

linear_reg(penalty = 1) %>% set_engine("glmnet") %>% translate()

linear_reg() %>% set_engine("stan") %>% translate()

lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # Recall that Sale_Price has been pre-logged
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )

lm_form_fit
lm_xy_fit

arg_info <- 
  tribble(
    ~ `Argument Type`, ~parsnip,
    "# trees", "trees",
    "# sampled predictors", "mtry",
    "# data points to split", "min_n"
  )

arg_info <-
  get_from_env("rand_forest_args") %>% 
  select(engine, parsnip, original) %>% 
  full_join(arg_info, by = "parsnip") %>% 
  mutate(package = ifelse(engine == "spark", "sparklyr", engine))

arg_info %>%
  select(package, `Argument Type`, original) %>%
  # mutate(original = paste0("<tt>", original, "</tt>")) %>% 
  pivot_wider(
    id_cols = c(`Argument Type`),
    values_from = c(original),
    names_from = c(package)
  ) %>% 
  kable(
    caption = "Example argument names for different random forest functions.",
    label = "rand-forest-args",
    escape = FALSE
  ) %>%
  kable_styling() %>%
  column_spec(2:4, monospace = TRUE)

arg_info %>%
  select(`Argument Type`, parsnip) %>%
  distinct() %>% 
  # mutate(parsnip = paste0("<tt>", parsnip, "</tt>")) %>% 
  kable(
    caption = "Random forest argument names used by parsnip.",
    label = "parsnip-args",
    escape = FALSE
  ) %>% 
  kable_styling(full_width = FALSE) %>%
  column_spec(2, monospace = TRUE)

rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  translate()

rand_forest(trees = 1000, min_n = 5) %>% 
  set_engine("ranger", verbose = TRUE) %>% 
  set_mode("regression") 

lm_form_fit %>% extract_fit_engine()

lm_form_fit %>% extract_fit_engine() %>% vcov()

model_res <- 
  lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)
param_est

tidy(lm_form_fit)

ames_test_small <- ames_test %>% slice(1:5)
predict(lm_form_fit, new_data = ames_test_small)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int")) 

tribble(
  ~ `Type of Prediction`, ~ `Returns a:`,
  "numeric",                 "numeric matrix",
  "class",                   "character matrix",
  "probability (2 classes)", "numeric matrix (2nd level only)",
  "probability (3+ classes)", "3D numeric array (all levels)", 
) %>% 
  kable(
    caption = "Different return values for glmnet prediction types.",
    label = "predict-types"
  ) %>% 
  kable_styling(full_width = FALSE)

tribble(
  ~ `type value`, ~ `column name(s)`,
  "numeric", ".pred",
  "class", ".pred_class",
  "prob", ".pred_{class levels}",
  "conf_int", ".pred_lower, .pred_upper",
  "pred_int", ".pred_lower, .pred_upper"
) %>% 
  kable(
    caption = "The tidymodels mapping of prediction types and column names.",
    label = "predictable-column-names",
  ) %>% 
  kable_styling(full_width = FALSE)  %>%
  column_spec(1:2, monospace = TRUE)

tree_model <- 
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tree_fit <- 
  tree_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(tree_fit, ames_test_small))

library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

lm_model <- linear_reg() %>% set_engine("lm")