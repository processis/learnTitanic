#tidyverse in r 

install.packages("tidyverse")
install.packages("lubridate")
install.packages("nycflights13")
library(tidyverse)
library(lubridate)
library(nycflights13)

#1. Create a new column basis count option

flights %>%
  mutate(long_flight = (air_time >= 6 * 60)) %>%
  View()

flights %>%
  mutate(long_flight = (air_time >= 6 * 60)) %>%
  count(long_flight)

flights %>%
  count(long_flight = air_time >= 6 * 60)

flights %>%
  count(flight_path = str_c(origin, " -> ", dest), sort = TRUE)

#2. Create a new column basis group by

flights %>%
  group_by(date = make_date(year, month, day)) %>%
  summarise(flights_n = n(), air_time_mean = mean(air_time, na.rm = TRUE)) %>%
  ungroup()

#3. Randomly Shuffle the data

flights %>%
  slice_sample(n = 15)

flights %>%
  slice_sample(prop = 0.15)

#4. Date column creation

flights %>%
  select(year, month, day) %>%
  mutate(date = make_date(year, month, day))

#5. Number Parsing

numbers_1 <- tibble(number = c("#1", "Number8", "How are you 3"))
numbers_1 %>% mutate(number = parse_number(number))

#6. Select columns with starts_with and ends_with

flights %>%
  select(starts_with("dep_"))
flights %>%
  select(ends_with("hour"))
flights %>%
  select(contains("hour"))

#7. case_when to create when conditions are met

flights %>%
  mutate(origin = case_when(
    (origin == "EWR") & dep_delay > 20 ~ "Newark International Airport - DELAYED",
    (origin == "EWR") & dep_delay <= 20 ~ "Newark International Airport - ON TIME DEPARTURE",
  )) %>%
  count(origin)

#8. str_replace_all to find and replace multiple options at once

flights %>%
  mutate(origin = str_replace_all(origin, c(
    "^EWR$" = "Newark International",    "^JFK$" = "John F. Kennedy International"
  ))) %>%
  count(origin)

#9. Filter groups without making a new column

flights_top_carriers <- flights %>%
  group_by(carrier) %>%
  filter(n() >= 10000) %>%
  ungroup()

#10. Extract rows from the first table which are matched in the second table
beginning_with_am<- airlines %>%   
  filter(name %>% str_detect("^Am")) 

#11. Extract rows from the first table which are not matched in the second table

flights %>%
  anti_join(airways_beginning_with_a, by = "carrier")

#12. fct_reorder to sort for charts creation

airline_names <- flights %>%
  left_join(airlines, by = "carrier")
airline_names %>%
  count(name) %>%
  ggplot(aes(name, n)) +
  geom_col()
airline_names %>%
  count(name) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(name, n)) +
  geom_col()

# 13. coord_flip to display counts more accurately

flights_with_airline_names %>%   
  count(name) %>%   
  mutate(name = fct_reorder(name, n)) %>%   
  ggplot(aes(name, n)) +   
  geom_col() +   
  coord_flip() 

#14. Generate all combinations using crossing

crossing(
  customer_channel = c("Bus", "Car"),
  customer_status = c("New", "Repeat"),
  spend_range = c("$0-$10", "$10-$20", "$20-$50", "$50+"))

#15. Group by based on function

summary <- function(data, col_names, na.rm = TRUE) {
  data %>%
    summarise(across({{ col_names }},
                     list(
                       min = min,
                       max = max,
                       median = median,
                       mean = mean
                     ),
                     na.rm = na.rm,
                     .names = "{col}_{fn}"
    ))
}
flights_with_airline_names %>%
  summary(c(air_time, arr_delay))
flights_with_airline_names %>%
  group_by(carrier) %>%
  summary(c(air_time, arr_delay))