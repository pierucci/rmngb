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
    eval(expression(plot(d, ...)), envir = environment(x))
}
