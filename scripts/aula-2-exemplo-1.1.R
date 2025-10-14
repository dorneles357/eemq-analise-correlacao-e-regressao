X1 <- c(30, 32, 24, 30, 26, 35, 25, 23, 35, 31)
X2 <- c(145, 150, 125, 157, 127, 140, 132, 107, 155, 145)
X3 <- c(7, 10, 7, 11, 8, 10, 10, 6, 12, 9)

cor(X1, X2)
cor(X1, X3)
cor(X2, X3)

ppcor::pcor.test(X1, X2, X3)
ppcor::pcor.test(X2, X3, X1)
ppcor::pcor.test(X1, X3, X2)