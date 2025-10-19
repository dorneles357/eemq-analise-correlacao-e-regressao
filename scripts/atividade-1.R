# DADOS

x <- c(0.647, 0.645, 0.650, 0.642, 0.643,
       0.651, 0.640, 0.651, 0.648, 0.642,
       0.643, 0.645, 0.639, 0.633, 0.642,
       0.648, 0.639, 0.638, 0.641, 0.636,
       0.638, 0.650, 0.649, 0.639, 0.633,
       0.635)
y <- c(0.43, 0.42, 0.44, 0.43, 0.42, 0.46,
       0.43, 0.44, 0.43, 0.42, 0.41, 0.41,
       0.40, 0.39, 0.40, 0.42, 0.40, 0.40,
       0.41, 0.39, 0.39, 0.42, 0.43, 0.40,
       0.39, 0.39)

# UTILITARIOS

n <- if (length(x) == length(y)){
  length(x)
}else {
  stop("Vetores de tamanhos diferentes")
}

sxy <- sum(x * y) - (sum(x) * sum(y)) / n

sxx <- sum(x^2) - (sum(x)^2) / n

syy <- sum(y^2) - (sum(y)^2) / n

b <- sxy / sxx

a <- mean(y) - b * mean(x)

t_tab <- qt(0.025, df = n - 2, lower.tail = FALSE)

s <- sqrt((syy - b * sxy) / (n - 2))

# EXERCICIO 1

r <- sqrt(b * sxy / syy)
r

# EXERCICIO 2

rc_for_correlation <- function(value) {
  if (value <= - t_tab || value >= t_tab) {
    "Rejeita H0: Não existe correlação na população"
  } else {
    "Não rejeita H0: Existe correlação na população"
  }
}

t_correlation <- r * sqrt((n - 2) / (1 - r^2))

t_correlation
rc_for_correlation(t_correlation)

# EXERCICIO 3

if (abs(r) >= 0.2 && abs(r) < 0.4) {
  "Correlação fraca"
} else if (abs(r) >= 0.4 && abs(r) < 0.7) {
  "Correlação moderada"
} else if (abs(r) >= 0.7 && abs(r) < 0.9) {
  "Correlação forte"
} else {
  "Sem correlação"
}

# EXERCICIO 4

equation  <- function(x) {
  a + b * x
}

rc_for_parameter <- function(value) {
  if (value <= - t_tab || value >= t_tab) {
    "Rejeita H0: O parâmetro é não significativo"
  } else {
    "Não rejeita H0: O parâmetro é significativo"
  }
}

t_b <- (b - 0) / (s / sqrt(sxx))

t_b
rc_for_parameter(t_b)

# EXERCICIO 5

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_abline(slope = b, intercept = a, color = "red")

# EXERCICIO 6

r2 <- ((b^2 * sxx) / (syy)) * 100
r2

# EXERCICIO 7

b

# EXERCICIO 8

equation(0.60)

# EXERCICIO 9

ic_a  <-  c(
  a - t_tab * s * sqrt(1 / n + (mean(x)^2) / sxx),
  a + t_tab * s * sqrt(1 / n + (mean(x)^2) / sxx)
)
ic_a

ic_b <- c(
  b - t_tab * s * sqrt(1 / sxx),
  b + t_tab * s * sqrt(1 / sxx)
)
ic_b

# EXERCICIO 10

ic_yx <- c(
  equation(0.60) - t_tab * s * sqrt(1 + 1 / n + ((0.60 - mean(x))^2) / sxx),
  equation(0.60) + t_tab * s * sqrt(1 + 1 / n + ((0.60 - mean(x))^2) / sxx)
)
ic_yx

# EXERCICIO 11

ic_eyx <- c(
  equation(0.60) - t_tab * s * sqrt(1 / n + ((0.60 - mean(x))^2) / sxx),
  equation(0.60) + t_tab * s * sqrt(1 / n + ((0.60 - mean(x))^2) / sxx)
)
ic_eyx

