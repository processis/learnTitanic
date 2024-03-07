library(tidymodels)
library(kableExtra)

tidymodels_prefer()

val_list <- function(x) {
  x <- format(table(x), big.mark = ",")
  x <- paste0("`", names(x), "` ($n = ", unname(x), "$)")
  knitr::combine_words(x)
}

source("ames_snippets.R")

lm_wflow <- 
  lm_wflow %>% 
  remove_recipe() %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))

lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type, data = ames)

library(tidymodels) # Includes the recipes package
tidymodels_prefer()

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())
simple_ames

lm_wflow %>% 
  add_recipe(simple_ames)

lm_wflow <- 
  lm_wflow %>% 
  remove_variables() %>% 
  add_recipe(simple_ames)
lm_wflow


lm_fit <- fit(lm_wflow, ames_train)
predict(lm_fit, ames_test %>% slice(1:3))
lm_fit %>% 
  extract_recipe(estimated = TRUE)

# To tidy the model fit: 
lm_fit %>% 
  # This returns the parsnip object:
  extract_fit_parsnip() %>% 
  # Now tidy the linear model object:
  tidy() %>% 
  slice(1:5)

#set. The most homes are in North Ames while the Greens, Green Hills, and Landmark neighborhood have very few instances."
ggplot(ames_train, aes(y = Neighborhood)) + 
  geom_bar() + 
  labs(y = NULL)

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors())

show_rows <- 
  ames_train %>% 
  mutate(.row = row_number()) %>% 
  group_by(Bldg_Type) %>% dplyr::select(Bldg_Type, .row) %>% 
  slice(1) %>% 
  pull(.row)
recipe(~Bldg_Type, data = ames_train) %>% 
  step_mutate(`Raw Data` = Bldg_Type) %>% 
  step_dummy(Bldg_Type, naming = function(var, lvl, ordinal = FALSE, sep = "_") lvl) %>% 
  prep() %>% 
  bake(ames_train) %>% 
  slice(show_rows) %>% 
  arrange(`Raw Data`) %>% 
  kable(
    caption = 'Illustration of binary encodings (i.e., dummy variables) for a qualitative predictor.',
    label = "dummy-vars"
  ) %>% 
  kable_styling(full_width = FALSE)

ggplot(ames_train, aes(x = Gr_Liv_Area, y = 10^Sale_Price)) + 
  geom_point(alpha = .2) + 
  facet_wrap(~ Bldg_Type) + 
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color = "lightblue") + 
  scale_x_log10() + 
  scale_y_log10() + 
  labs(x = "Gross Living Area", y = "Sale Price (USD)")

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  # Gr_Liv_Area is on the log scale from a previous step
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") )

library(patchwork)
library(splines)

plot_smoother <- function(deg_free) {
  ggplot(ames_train, aes(x = Latitude, y = 10^Sale_Price)) + 
    geom_point(alpha = .2) + 
    scale_y_log10() +
    geom_smooth(
      method = lm,
      formula = y ~ ns(x, df = deg_free),
      color = "lightblue",
      se = FALSE
    ) +
    labs(title = paste(deg_free, "Spline Terms"),
         y = "Sale Price (USD)")
}

( plot_smoother(2) + plot_smoother(5) ) / ( plot_smoother(20) + plot_smoother(100) )

recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude,
       data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, deg_free = 20)

library(recipes)
library(themis)

preps <- as.character(methods("prep"))
steps <- gsub("prep\\.", "", preps)
steps <- grep("^step", steps, value = TRUE)

skip <- rep(rlang::na_lgl, length(steps))
for (i in seq_along(skip)) {
  x_code <- try(getFromNamespace(steps[i], "recipes"), silent = TRUE)
  if (inherits(x_code, "try-error")) {
    x_code <- try(getFromNamespace(steps[i], "themis"), silent = TRUE)
  }
  if (!inherits(x_code, "try-error")) {
    skip[i] <- formals(x_code)$skip
  }
}

skip_list <- paste0("`", steps[skip], "()`")

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

tidy(ames_rec)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01, id = "my_id") %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

estimated_recipe <- 
  lm_fit %>% 
  extract_recipe(estimated = TRUE)

tidy(estimated_recipe, id = "my_id")

tidy(estimated_recipe, number = 2)

library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)

if(is_new_version(lm_fit, "RData/lm_fit.RData")) {
  save(lm_fit, file = "RData/lm_fit.RData", version = 2, compress = "xz")
}