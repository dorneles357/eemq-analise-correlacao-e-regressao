a <- c(7, 4, 2, 6, 1, 3, 8, 5)
b <- c(44, 72, 69, 70, 93, 82, 67, 80)

cor(a, b, method = c("spearman"))

cor.test(
  a,
  b,
  alternative = c("two.sided"),
  method = c("spearman"),
  exact = NULL,
  conf.level = 0.95
)