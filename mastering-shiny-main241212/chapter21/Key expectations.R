complicated_object <- list(
  x = list(mtcars, iris),
  y = 10
)
expect_equal(complicated_object$y, 10)

expect_error("Hi!")
#> Error: "Hi!" did not throw the expected error.
expect_error(stop("Bye"))

f <- function() {
  stop("Calculation failed [location 1]")
}

expect_error(f(), "Calculation failed [location 1]")
#> Error in f(): Calculation failed [location 1]
expect_error(f(), "Calculation failed \\[location 1\\]")


