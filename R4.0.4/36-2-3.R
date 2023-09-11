#36.2.3.1 Getting Started in R with ggvis
library(titanic)
library(tidyverse)
library(ggvis)
# for the data
# for the tibble
# for the plot

titanic_train$Age%>%
  as_tibble%>%
  na.omit%>%
  ggvis(x = ~value) %>%
  layer_densities(
    adjust = input_slider(.1, 2, value = 1, step = .1,
                          label = "Bandwidth"),
    kernel = input_select(
      c("Gaussian"
        = "gaussian",
        "Epanechnikov" = "epanechnikov",
        "Rectangular" = "rectangular",
        "Triangular"
        = "triangular",
        "Biweight"
        = "biweight",
        "Cosine"
        = "cosine",
        "Optcosine"
        = "optcosine"),
      label = "Kernel")
  )


#36.2.3.2 Combining the Power of ggvis and Shiny

library(ggvis)
function(input, output, session) {
  # A reactive subset of mtcars:
  reacCars <- reactive({ mtcars[1:input$n, ] })
  # Register observers and place the controls:
  reacCars %>%
    ggvis(~wt, ~mpg, fill=(~cyl)) %>%
    layer_points() %>%
    layer_smooths(span = input_slider(0.5, 1, value = 1,
                                      label = 'smoothing span:')) %>%
    bind_shiny("plot1", "plot_ui_div")
  output$carsData <- renderTable({ reacCars()[, c("wt", "mpg")] })
}

library(ggvis)
fluidPage(sidebarLayout(
  sidebarPanel(
    # Explicit code for a slider-bar:
    sliderInput("n", "Number of points", min = 1, max = nrow(mtcars),
                value = 10, step = 1),
    # No code needed for the smoothing span, ggvis does this:
    uiOutput("plot_ui_div") # produces a <div> with id corresponding
    # to argument in bind_shiny
  ),
  mainPanel(
    # Place the plot "plot1" here:
    ggvisOutput("plot1"),
    # matches argument to bind_shiny()
    # Under this the table of selected card models:
    tableOutput("carsData") # parses the result of renderTable()
  )
))

shiny::runApp("/path/to/my/app")

# remove.packages("Dplyr")
# remove.packages("tidyr")
# remove.packages("stringr")
# remove.packages("tibble")


# install.packages("googledrive")
# install.packages("googlesheets4")
# install.packages("httr")
# install.packages("ragg")
# install.packages("rvest")


install.packages("tidyverse")