rDist <- function(x, ...) {
    x <- na.omit(x)
    d <- density(x, ...)
    
    structure(function(n)
        rnorm(n = n,
              mean = sample(x = x, size = n, replace = TRUE),
              sd = d$bw),
              class = "rDist")
}
plot.rDist <- function(x, ...) {
  denObj <- eval(expression(d), envir = environment(x))
  plot(denObj, ...)
}
