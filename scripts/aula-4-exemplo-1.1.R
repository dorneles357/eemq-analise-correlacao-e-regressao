# data from the example
y <- c(12, 3, 11, 1, 13, 20, 2, 25, 26, 15, 1, 15, 11, 5, 7);
x1 <- c(18, 16, 25, 12, 20, 35, 17, 25, 39, 20, 18, 29, 20, 16, 29);
x2 <- c(2, 3, 2, 3, 3, 2, 1.5, 5, 1, 2.5, 2, 3.5, 5, 1, 1.5);

df <- data.frame(y, x1, x2)

# Fit the linear model using lm()
model <- lm(y ~ x1 + x2, data = df)
summary(model)

# Manual calculation of beta coefficients
matrix_y <- matrix(y, ncol = 1)
matrix_x <- matrix(cbind(1, x1, x2), ncol = 3)
matrix_xt <- t(matrix_x)

matrix_x_xt <- matrix_xt %*% matrix_x
matrix_x_xt_inv <- solve(matrix_x_xt)

beta_hat <- matrix_x_xt_inv %*% matrix_xt %*% matrix_y
beta_hat

# ANOVA table
matrix_yt <- t(matrix_y)

gl_total <- length(y) - 1
gl_model <- length(coef(model)) - 1
gl_residual <- gl_total - gl_model

ss_total <- matrix_yt %*% matrix_y - (sum(y)^2) / length(y)
ss_model <- t(beta_hat) %*% (matrix_xt %*% matrix_y) - (sum(y)^2) / length(y)
ss_residual <- ss_total - ss_model

ms_model <- ss_model / gl_model
ms_residual <- ss_residual / gl_residual

f_cal <- ms_model / ms_residual
f_tab <- qf(0.95, gl_model, gl_residual)

f_table <- data.frame(
  Source = c("Model", "Residuals", "Total"),
  SS = c(ss_model, ss_residual, ss_total),
  df = c(gl_model, gl_residual, gl_total),
  MS = c(ms_model, ms_residual, NA),
  F_cal = c(f_cal, NA, NA),
  F_tab = c(f_tab, NA, NA)
)
f_table