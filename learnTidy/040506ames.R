library(tidyverse)
library(modeldata) # This is also loaded by the tidymodels package
data(ames)
dim(ames)
library(tidymodels)
tidymodels_prefer()
ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col= "white")

ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col= "white") +
  scale_x_log10()
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))
#Continue with Ch 5
# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(501)

# Save the split information for an 80/20 split of the data
ames_split <- initial_split(ames, prop = 0.80)
ames_split
# to get the data sets
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)
#

q# more code fr ch5
sale_dens <- 
  density(ames$Sale_Price, n = 2^10) %>% 
  tidy() 
quartiles <- quantile(ames$Sale_Price, probs = c(1:3)/4)
quartiles <- tibble(prob = (1:3/4), value = unname(quartiles))
quartiles$y <- approx(sale_dens$x, sale_dens$y, xout = quartiles$value)$y

quart_plot <-
  ggplot(ames, aes(x = Sale_Price)) +
  geom_line(stat = "density") +
  geom_segment(data = quartiles,
               aes(x = value, xend = value, y = 0, yend = y),
               lty = 2) +
  labs(x = "Sale Price (log-10 USD)", y = NULL)
quart_plot

install.packages("tidyverse")
#continue Ch6  Fitting models
library(tidymodels)
library(kknn)
library(kableExtra)
library(tidyr)

