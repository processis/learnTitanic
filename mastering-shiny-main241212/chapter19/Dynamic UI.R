#Dynamic UI

filterUI <- function(id) {
  uiOutput(NS(id, "controls"))
}

library(purrr)
make_ui <- function(x, id, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(id, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectInput(id, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}
filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}


filterServer <- function(id, df) {
  stopifnot(is.reactive(df))
  moduleServer(id, function(input, output, session) {
    vars <- reactive(names(df()))
    output$controls <- renderUI({
      map(vars(), function(var) make_ui(df()[[var]], NS(id, var), var))
    })
    reactive({
      each_var <- map(vars(), function(var) filter_var(df()[[var]], input[[var]]))
      reduce(each_var, `&`)
    })
  })
}

filterApp <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        datasetInput("data", is.data.frame),
        textOutput("n"),
        filterUI("filter"),
      ),
      mainPanel(
        tableOutput("table")
      )
    )
    server <- function(input, output, session) {
      df <- datasetServer("data")
      filter <- filterServer("filter", df)
      output$table <- renderTable(df()[filter(), , drop = FALSE])
      output$n <- renderText(paste0(sum(filter()), " rows"))
    }
    shinyApp(ui, server)
}

