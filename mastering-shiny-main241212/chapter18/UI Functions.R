#Functional Programming

library(purrr)
vars <- c("alpha", "beta", "gamma", "delta")
sliders <- map(vars, sliderInput01)
ui <- fluidRow(sliders)

# UI as Data

vars <- tibble::tribble(
  ~ id,   ~ min, ~ max,
  "alpha",     
  0,     
  "beta",      
  "gamma",    
  "delta",     
)

mySliderInput <- function(id, label = id, min = 0, max = 1) {
  sliderInput(id, label, min = min, max = max, value = 0.5, step = 0.1)
}

sliders <- pmap(vars, mySliderInput)


# Server Functions

# Reading Uploaded Data

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ","),
           tsv = vroom::vroom(input$file$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  output$head <- renderTable({
    head(data(), input$n)
  })
}

load_file <- function(name, path) {
  ext <- tools::file_ext(name)
  switch(ext,
         csv = vroom::vroom(path, delim = ","),
         tsv = vroom::vroom(path, delim = "\t"),
         validate("Invalid file; Please upload a .csv or .tsv file")
)
}
  
  server <- function(input, output, session) {
    data <- reactive({
      req(input$file)
      load_file(input$file$name, input$file$datapath)
    })
    output$head <- renderTable({
      head(data(), input$n)
    })
  }
  

  # Internal Functions
  
  server <- function(input, output, session) {
    switch_page <- function(i) {
      updateTabsetPanel(input = "wizard", selected = paste0("page_", i))
    }
    observeEvent(input$page_12, switch_page(2))
    observeEvent(input$page_21, switch_page(1))
    observeEvent(input$page_23, switch_page(3))
    observeEvent(input$page_32, switch_page(2))
  }
  
  switch_page <- function(i) {
    updateTabsetPanel(input = "wizard", selected = paste0("page_", i))
  }
  server <- function(input, output, session) {
    observeEvent(input$page_12, switch_page(2))
    observeEvent(input$page_21, switch_page(1))
    observeEvent(input$page_23, switch_page(3))
    observeEvent(input$page_32, switch_page(2))
  }
