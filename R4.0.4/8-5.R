#8.5 Creating an Overview of Data Characteristics

N <- 100
t <- data.frame(id = 1:N, result = rnorm(N))
summary(t)

library(tidyverse) # not only for %>% but also for group_by, etc.
# In mtcars the type of the car is only in the column names,
# so we need to extract it to add it to the data
n <- rownames(mtcars)
# Now, add a column brand (use the first letters of the type)
t <- mtcars %>%
  mutate(brand = str_sub(n, 1, 4))
# add column

# First, we need to find out which are the most abundant brands
# in our dataset (set cutoff at 2: at least 2 cars in database)
top_brands <- count(t, brand) %>% filter(n >= 2)
# top_brands is not simplified to a vector in the tidyverse
print(top_brands)

grouped_cars <- t
filter(brand %in% top_brands$brand)
group_by(brand)
summarise(
  avgDSP = round(mean(disp), 1),
  avgCYL = round(mean(cyl), 1),
  minMPG = min(mpg),
  medMPG = median(mpg),
  avgMPG = round(mean(mpg),2),
  maxMPG = max(mpg),
)
print(grouped_cars)

by_vs_am <- mtcars %>% group_by(vs, am)
by_vs <- by_vs_am %>% summarise(n = n())
by_vs

by_vs %>% summarise(n = sum(n))

# To removing grouping, use ungroup:
by_vs %>%
  ungroup() %>%
  summarise(n = sum(n))

# You can group by expressions: this is just short-hand for
# a mutate/rename followed by a simple group_by:
mtcars %>% group_by(vsam = vs + am)

# By default, group_by overrides existing grouping:
mtcars%>%
  group_by(cyl)%>%
  group_by(vs, am) %>%
  group_vars()

# Use add = TRUE to append grouping levels:
mtcars%>%
  group_by(cyl)%>%
  group_by(vs, am, add = TRUE) %>%
  group_vars()