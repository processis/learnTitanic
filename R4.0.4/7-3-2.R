#7.3.2Piping with R

t <- tibble("x" = runif(10))
t <- within(t, y <- 2 * x + 4 + rnorm(10, mean=0,sd=0.5))

t <- tibble("x" = runif(10)) %>%
  within(y <- 2 * x + 4 + rnorm(10, mean=0,sd=0.5))

a <- c(1:10)
a %>% mean()

a %>% mean

mean(a)


# f1
# Dummy function that from which only the error throwing part
# is shown.
f1 <- function() {
  # Here goes the long code that might be doing something risky
  # (e.g. connecting to a database, uploading file, etc.)
  # and finally, if it goes wrong:
  stop("Early exit from f1!") # throw error
}
tryCatch(f1(),
         # the function to try
         error
         = function(e) {paste("_ERROR_:",e)},
         warning = function(w) {paste("_WARNING_:",w)},
         message = function(m) {paste("_MESSSAGE_:",m)},
         finally="Last command"
         # do at the end
)

# and finally, if it goes wrong:
stop("Early exit from f1!") # something went wrong
}%>%
  tryCatch(
    error
    = function(e) {paste("_ERROR_:",e)},
    warning = function(w) {paste("_WARNING_:",w)},
    message = function(m) {paste("_MESSSAGE_:",m)},
    finally="Last command"
  )
# Note that it fails in silence.
# do at


