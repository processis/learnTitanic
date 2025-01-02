library(shiny)
reactiveConsole(TRUE)

# Reactive Values

x <- reactiveVal(10)
x()       
# get
#> [1] 10
x(20)     
x()       
# set
# get

#> [1] 20
r <- reactiveValues(x = 10)
r$x       
# get
#> [1] 10
r$x <- 20 # set
r$x       
# get
#> [1] 20

a1 <- a2 <- 10
a2 <- 20
a1 # unchanged
#> [1] 10

b1 <- b2 <- reactiveValues(x = 10)
b1$x <- 20
b2$x


# Exercises
l1 <- reactiveValues(a = 1, b = 2)
l2 <- list(a = reactiveVal(1), b = reactiveVal(2))

#Exercises
ui <- fluidPage(
  checkboxInput("error", "error?"),
  textOutput("result")
)
server <- function(input, output, session) {
  a <- reactive({
    if (input$error) {
      stop("Error!")
    } else {
      1
    }
  })
  b <- reactive(a() + 1)
  c <- reactive(b() + 1)
  output$result <- renderText(c())
}

# Observers and Outputs

y <- reactiveVal(10)
observe({
  message("`y` is ", y())
})
#> Warning: Error in y: could not find function "y"
y(5)
y(4)

x <- reactiveVal(1)
y <- observe({
  x()
  observe(print(x()))
})
#> Warning: Error in x: could not find function "x"
x(2)
x(3)

# Isolating Code

# isolate()
r <- reactiveValues(count = 0, x = 1)
observe({
  r$x
  r$count <- r$count + 1
})

r <- reactiveValues(count = 0, x = 1)
class(r)
#> [1] "rv_flush_on_write" "reactivevalues"
observe({
  r$x
  r$count <- isolate(r$count) + 1
})
#> Warning: Error in <observer>: object 'r' not found

r$x <- 1
r$x <- 2
r$count
#> [1] 0
r$x <- 3
r$count
#> [1] 0

# observeEvent() and eventReactive()

observeEvent(x(), {
  count(count() + 1)
})

#Exercises

ui <- fluidPage(
  numericInput("x", "x", value = 50, min = 0, max = 100),
  actionButton("capture", "capture"),
  textOutput("out")
)

# Timed Invalidation

x <- reactive({
  invalidateLater(500)
  rnorm(10)
})

sum <- reactiveVal(0)
observe({
  invalidateLater(300)
  sum(isolate(sum()) + runif(1))
})

#Polling

data <- reactive({
  on.exit(invalidateLater(1000))
  read.csv("data.csv")
})

server <- function(input, output, session) {
  data <- reactivePoll(1000, session,
                       function() file.mtime("data.csv"),
                       function() read.csv("data.csv")
  )
}

server <- function(input, output, session) {
  data <- reactiveFileReader(1000, session, "data.csv", read.csv)
}

# Long-Running Reactives

x <- reactive({
  invalidateLater(500)
  Sys.sleep(1)
  10
})

x <- reactive({
  on.exit(invalidateLater(500), add = TRUE)
  Sys.sleep(1)
  10
})

# Timer Accuracy

velocity <- 3
r <- reactiveValues(distance = 1)
last <- proc.time()[[3]]
observe({
  cur <- proc.time()[[3]]
  time <- last - cur
  last <<- cur
  r$distance <- isolate(r$distance) + velocity * time
  invalidateLater(100)
})

