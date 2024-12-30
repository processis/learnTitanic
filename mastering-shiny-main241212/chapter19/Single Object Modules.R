# Single Object Modules

histogramUI <- function(id, df) {
  tagList(
    selectInput(NS(id, "var"), "Variable", names(df)),
    numericInput(NS(id, "bins"), "bins", 10, min = 1),
    plotOutput(NS(id, "hist"))
  )
}
histogramServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(df[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
  })
}

ui <- fluidPage(
  tabsetPanel(
    tabPanel("mtcars", histogramUI("mtcars", mtcars)),
    tabPanel("iris", histogramUI("iris", iris))
  )
)
server <- function(input, output, session) {
  histogramServer("mtcars", mtcars)
  histogramServer("iris", iris)
}

histogramApp <- function(id, df) {
  list(
    ui = histogramUI(id, df),
    server = histogramServer(id, df)
  )
}

hist1 <- histogramApp("mtcars", mtcars)
hist2 <- histogramApp("iris", iris)
ui <- fluidPage(
  tabsetPanel(
    tabPanel("mtcars", hist1$ui()),
    tabPanel("iris", hist2$ui())
  )
)
server <- function(input, output, session) {
  hist1$server()
  hist2$server()
}