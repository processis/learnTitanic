# A Package

library(usethis)


library(shiny)
monthApp <- function(...) {
  stones <- vroom::vroom("/media/user/娱乐/learnTitanic/mastering-shiny-main241212/birthstones.csv")
  months <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )
  ui <- navbarPage(
    "Sample app",
    tabPanel("Pick a month",
             selectInput("month", "What's your favourite month?", choices = months)
    ),
    tabPanel("Feedback", monthFeedbackUI("tab1")),
    tabPanel("Birthstone", birthstoneUI("tab2"))
  )
  server <- function(input, output, session) {
    monthFeedbackServer("tab1", reactive(input$month))
    birthstoneServer("tab2", reactive(input$month))
  }
  shinyApp(ui, server)
}


#Workflow

if (interactive()) {
  require(usethis, quietly = TRUE)
}

# Sharing

dataSummaryApp <- function(df) {
  ui <- fluidPage(
    selectInput("var", "Variable", choices = names(df)),
    verbatimTextOutput("summary")
  )
  server <- function(input, output, session) {
    output$summary <- renderPrint({
      summary(df[[input$var]])
    })
  }
  shinyApp(ui, server)
}


# Extra Steps

#Deploying Your App-Package

pkgload::load_all(".")
myApp()

usethis::use_package("shiny")
usethis::use_package("pkgload")
