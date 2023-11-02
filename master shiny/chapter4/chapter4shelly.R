#products <- vroom::vroom("/home/user/Desktop/shengaotiz1.tsv")
#products

#products <- read.table("/home/user/Desktop/shengaotiz1.xlsx",
 #                        sep = ",", header = TRUE)

# 4.4 Prototype 

desharnais <- read.table("/home/user/Desktop/desharnais77kaggle.csv",
                         sep = ",", header = TRUE)



selected <- injuries %>% filter(prod_code == 649)
nrow(selected)

selected %>% count(location, wt = weight, sort = TRUE)

selected %>% count(body_part, wt = weight, sort = TRUE)

selected %>% count(diag, wt = weight, sort = TRUE)


summary %>% 
  ggplot(aes(age, n, colour = sex)) + 
  geom_line() + 
  labs(y = "Estimated number of injuries")

summary <- selected %>% 
  count(age, sex, wt = weight) %>% 
  left_join(population, by = c("age", "sex")) %>% 
  mutate(rate = n / population * 1e4)

summary

summary %>% 
  ggplot(aes(age, rate, colour = sex)) + 
  geom_line(na.rm = TRUE) + 
  labs(y = "Injuries per 10,000 people")

selected %>% 
  sample_n(10) %>% 
  pull(narrative)
