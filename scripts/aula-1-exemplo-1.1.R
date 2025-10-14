x <- c(5, 8, 10, 12, 15)
y <- c(10, 30, 45, 50, 75)

cor(x, y)

cor.test(
  x,
  y,
  alternative = c("less"),
  method = c("pearson"),
  exact = NULL,
  conf.level = 0.95
)

cov(x, y)

var(x)
var(y)

rxy  <- cov(x, y) / ((var(x) * var(y))^0.5)
rxy