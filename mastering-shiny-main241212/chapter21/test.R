library(shiny)
library(testthat) # >= 3.0.0

library(assertthat)
library(crayon)
library(debugme)
library(digest)
library(htmlwidgets)
library(httpuv)
library(httr)
library(jsonlite)
library(parsedate)
library(pingr)
library(R6)
library(rematch)
library(rlang)
library(rstudioapi)
library(webdriver)
library(wither)

install.packages("pak")

library(shinytest)

# 21.1.2 Basic workflow

load_file <- function(name, path) {
  ext <- tools::file_ext(name)
  switch(ext,
         csv = vroom::vroom(path, delim = ",", col_types = list()),
         tsv = vroom::vroom(path, delim = "\t", col_types = list()),
         validate("Invalid file; Please upload a .csv or .tsv file")
  )
}


test_that("load_file() handles all input types", {
  # Create sample data
  df <- tibble::tibble(x = 1, y = 2)
  path_csv <- tempfile()
  path_tsv <- tempfile()
  write.csv(df, path_csv, row.names = FALSE)
  write.table(df, path_tsv, sep = "\t", row.names = FALSE)

  expect_equal(load_file("test.csv", path_csv), df)
  expect_equal(load_file("test.tsv", path_tsv), df)
  expect_error(load_file("blah", path_csv), "Invalid file")
})
#> Test passed 😀
