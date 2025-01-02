# Exercises

library(reactlog)
library(shiny)

server <- function(input, output, session) {
  sum <- reactive(input$x + input$y + input$z)
  prod <- reactive(input$x * input$y * input$z)
  division <- reactive(prod() / sum())
}

x1 <- reactiveVal(1)
x2 <- reactiveVal(2)
x3 <- reactiveVal(3)


y1 <- reactive({
  Sys.sleep(1)
  x1()
})
y2 <- reactive({
  Sys.sleep(1)
  x2()
})
y3 <- reactive({
  Sys.sleep(1)
  x2() + x3() + y2() + y2()
})
observe({
  print(y1())
  print(y2())
  print(y3())
})

x <- reactiveVal(1)
y <- reactive(x + y())
y()


# Dynamism

ui <- fluidPage(
  selectInput("choice", "A or B?", c("a", "b")),
  numericInput("a", "a", 0),
  numericInput("b", "b", 10),
  textOutput("out")
)
server <- function(input, output, session) {
  output$out <- renderText({
    if (input$choice == "a") {
      input$a
    } else {
      input$b
    }
  })
}

output$out <- renderText({
  a <- input$a
  b <- input$b
  if (input$choice == "a") {
    a
  } else {
    b
  }
})
