#36.3
# diversity
# Calculates the entropy of a system with equiprobable states.
# Arguments:
#x -- numeric vector -- observed probabilities of classes
# Returns:
#numeric -- the entropy / diversity measure
diversity <- function(x) {
  f <- function(x) x * log(x)
  x1 <- mapply(FUN = f, x)
  - sum(x1) / log(length(x))
}

# diversity
# Calculates the entropy of a system with discrete states.
# Arguments:
#x-- numeric vector -- observed probabilities of classes
#prior -- numeric vector -- prior probabilities of the classes
# Returns:
#numeric -- the entropy / diversity measure
diversity <- function(x, prior = NULL) {
  if (min(x) <= 0) {return(0);} # the log will fail for 0
  # If the numbers are higher than 1, then not probabilities but
  # populations are given, so we rescale to probabilities:
  if (sum(x) != 1) {x <- x / sum(x)}
  N <- length(x)
  if(!is.null(prior)) {
    for (i in (1:N)) {
      a <- (1 - 1 / (N * prior[i])) / (1 - prior[i])
      b <- (1 - N * prior[i]^2) / (N * prior[i] * (1 - prior[i]))
      x[i] <- a * x[i]^2 + b * x[i]
    }
  }
  f <- function(x) x * log(x)
  x1 <- mapply(FUN = f, x)
  - sum(x1) / log(N)
}


# Consider the following prior probabilities:
pri <- c(0.1,0.5,0.4)
# No prior priorities supplied, so 1/N is most diverse:
diversity(c(0.1,0.5,0.4))

# The population matches prior probabilities, so index should be 1:
diversity(c(0.1,0.5,0.4), prior = pri)

# Very non-diverse population:

# Only one sub-group is represented (no diversity):
diversity(c(1,0,0), prior = pri)

# Numbers instead of probabilities provided, also this works:
diversity(c(100,150,200))


females <- seq(from = 0,to = 1, length.out = 100)
div <- numeric(0)
for (i in (1:length(females))) {
  div[i] <- diversity (c(females[i], 1 - females[i]))
}
d <- as.data.frame(cbind(females, div) )
colnames(d) <- c('percentage females', 'diversity index')
library(ggplot2)
p <- ggplot(data = d,
            aes(x = `percentage females`, y = `diversity index`)) +
  geom_line(color = 'red', lwd = 3) +
  ggtitle('Diversity Index') +
  xlab('percentage females') + ylab('diversity index')
p



library(tidyverse)
N <- 200
set.seed(1866)
d0 <- data.frame("ID"
                 = 1:N,
                 # Log-normal age distribution
                 "age"
                 = round(rlnorm(N, log(30), log(1.25))),
                 # A significant bias towards the old continent:
                 "continent"
                 = ifelse(runif(N) < 0.3, "America",
                          ifelse(runif(N) < 0.7,"Europe","Other")),
                 # A mild bias towards males:
                 "gender"
                 = ifelse(runif(N) < 0.45, "F", "M"),
                 # Grade will be filled in later:
                 "grade"
                 = 0,
                 # Three teams of different sizes:
                 "team"
                 = ifelse(runif(N) < 0.6, "bigTeam",
                          ifelse(runif(N) < 0.6,
                                 "mediumTeam",
                                 ifelse(runif(N) < 0.8, "smallTeam",
                                        "XsmallTeam"))),
                 # Most people have little people depending on them:
                 "dependents" = round(rlnorm(N,log(0.75),log(2.5))),
                 # Random performance (no bias linked, but different group sizes):
                 "performance" = ifelse(runif(N) < 0.1, "L",
                                        ifelse(runif(N) < 0.6, "M",
                                               ifelse(runif(N) < 0.7, "H", "XH"))),
                 # Salary will be filled in later:
                 "salary"
                 = 0,
                 # We make just a snapshot dashboard, so we do not need this now,
                 # but we could use this later to show evolution:
                 "timestamp"
                 = as.Date("2020-01-01")
)
# Now we clean up age and fill in grade, salary and lastPromoted without
# any bias for gender, origin -- but with a bias for age.
d1 <- d0%>%
  mutate(age
         = ifelse((age < 18), age + 10, age))%>%
  mutate(grade = ifelse(runif(N) * age < 20, 0,
                        ifelse(runif(N) * age < 25, 1,
                               ifelse(runif(N) * age < 30, 2, 3)))) %>%
  mutate(salary = round(exp(0.75 * grade) * 4000 +
                          rnorm(N,0,1500)))%>%
  mutate(lastPromoted = round(exp(0.05 * (3-grade)) * 1 +
                                abs(rnorm(N,0,5))) -1)


# If not done yet, install the package:
# install.packages('flexdashboard')
# Then load the package:
library(flexdashboard)
Overview
#========
  Row
-------------------------------------
#  ```{r}
# here goes the R-code to get data and calculate diversity indices.


#36.3.3 A Dashboard with shinydashboard
### Gender
#```{r genderGauge}
# ranges:
rGreen<- c(0.900001, 1)
rAmber<- c(0.800001, 0.9)
rRed<- c(0, 0.8)
iGender <- round(diversity(table(d1$gender)),3)
gauge(iGender, min = 0, max = 1, gaugeSectors(
  success = rGreen, warning = rAmber, danger = rRed
))
kable(table(d1$gender))


gauge <-gvisGauge(iGender,
          options=list(min = 0, max = 1,
                       greenFrom = 0.9,greenTo = 1,
                       yellowFrom = 0.8, yellowTo = 0.8,
                       redFrom = 0, redTo = 0.8,
                       width = 400, height = 300))

Gender
#======================================
  Row {.tabset}
-------------------------------------
  ### Composition
#  ```{r}
p2 <- ggplot(data = d1, aes(x=gender, fill=gender)) +
  geom_bar(stat="count", width=0.7) +
  facet_grid(rows=d1$grade) +
  ggtitle('workforce composition i.f.o. salary grade')
ggplotly(p2)
#```
### Tab2
#```{r RCodeForTab2}
# etc

### This is only the first gauge, the next one can be described below.


#36.3.3 A Dashboard with shinydashboard
# this file should be called app.R
library(shinydashboard)
# general code (not part of server or ui function)
my_seed <- 1865
set.seed(my_seed)
N <- 150
# user interface ui
ui <- dashboardPage(
  dashboardHeader(title = "ShinyDashBoard"),
  dashboardSidebar(
    title = "Choose ...",
    numericInput('N', 'Number of data points:', N),
    numericInput('my_seed', 'seed:', my_seed),
    sliderInput("bins", "Number of bins:",
                min = 1, max = 50, value = 30)
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("box1"),
      valueBoxOutput("box2")
    ),
    plotOutput("p1", height = 250)
  )
)
# server function
server <- function(input, output) {
  d <- reactive({
    set.seed(input$my_seed)
    rnorm(input$N)
  })
  output$p1 <- renderPlot({
    x <- d()
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'deepskyblue3', border = 'gray')
  })
  output$box1 <- renderValueBox({
    valueBox(
      value
      = formatC(mean(d()), digits = 2, format = "f"),
      subtitle = "mean", icon
      = icon("globe"),
      color
      = "light-blue")
  })
  output$box2 <- renderValueBox({
    valueBox(
      value
      = formatC(sd(d()), digits = 2, format = "f"),
      subtitle = "standard deviation", icon
      = icon("table"),
      color
      = "light-blue")
  })