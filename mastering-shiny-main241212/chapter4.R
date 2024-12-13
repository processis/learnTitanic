library(shiny)
library(vroom)
library(tidyverse)
library(ggplot2)

dir.create("neiss")
#> Warning in dir.create("neiss"): 'neiss' already exists
download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")


injuries <- vroom::vroom("/media/user/娱乐/mastering-shiny-main/chapter4/injuries.tsv.gz")
injuries

injuries <- vroom("/media/user/娱乐/mastering-shiny-main/chapter4/injuries.tsv.gz")
injuries

products <- vroom::vroom("/media/user/娱乐/mastering-shiny-main/chapter4/products.tsv")
products

population <- vroom::vroom("/media/user/娱乐/mastering-shiny-main/chapter4/population.tsv")
population

selected <- injuries %>% filter(prod_code == 649)
nrow(selected)

selected %>% count(location, wt = weight, sort = TRUE)

selected %>% count(body_part, wt = weight, sort = TRUE)


selected %>% count(diag, wt = weight, sort = TRUE)

summary <- selected %>%
  count(age, sex, wt = weight)
summary

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
  ggplot(aes(age,rate, colour = sex)) +
  geom_line(na.rm = TRUE) +
  labs(y = "Injuries per 10,000 people")

selected %>%
  sample_n(10) %>%
  pull(narrative)

# 4.4 Prototype
prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(
  fluidRow(
    column(6,
           selectInput("code", "Product", choices = prod_codes)
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(prod_code == input$code))

  output$diag <- renderTable(
    selected() %>% count(diag, wt = weight, sort = TRUE)
  )
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = TRUE)
  )
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = TRUE)
  )

  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })

  output$age_sex <- renderPlot({
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  }, res = 96)
}

#neiss/prototype.R 中找到源代码
#并在 https://hadley.shinyapps.io/ms-prototype/ 上试用该应用程序的实时版本。

#4.5 Polish tables

injuries %>%
  mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
  group_by(diag) %>%
  summarise(n = as.integer(sum(weight)))

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

#neiss/polish-tables.R and try out a live version of the app at https://hadley.shinyapps.io/ms-polish-tables.

# 4.6 Rate vs count

fluidRow(
  column(8,
         selectInput("code", "Product",
                     choices = setNames(products$prod_code, products$title),
                     width = "100%"
         )
  ),
  column(2, selectInput("y", "Y axis", c("rate", "count")))
)

output$age_sex <- renderPlot({
  if (input$y == "count") {
    summary() %>%
      ggplot(aes(age, n, colour = sex)) +
      geom_line() +
      labs(y = "Estimated number of injuries")
  } else {
    summary() %>%
      ggplot(aes(age, rate, colour = sex)) +
      geom_line(na.rm = TRUE) +
      labs(y = "Injuries per 10,000 people")
  }
}, res = 96)

#neiss/rate-vs-count.R and try out a live version of the app at https://hadley.shinyapps.io/ms-rate-vs-count.

# 4.7 Narrative
fluidRow(
  column(2, actionButton("story", "Tell me a story")),
  column(10, textOutput("narrative"))
)

narrative_sample <- eventReactive(
  list(input$story, selected()),
  selected() %>% pull(narrative) %>% sample(1)
)
output$narrative <- renderText(narrative_sample())

#neiss/narrative.R and try out a live version of the app at https://hadley.shinyapps.io/ms-narrative.
