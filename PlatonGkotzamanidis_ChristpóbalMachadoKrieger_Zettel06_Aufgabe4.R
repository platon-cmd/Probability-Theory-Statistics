## Platon Gkotzamanidis  
## Christóbal Marchado Krieger 

###################################################
## Aufgabe 4 (a)
###################################################
set.seed(420)

size <- 50
sigma_squared <- 100
beta_0 <- 6
beta_1 <- 9

Xi <- runif(size, min = 0, max = 10)

# Berechnung der Yi-Werte nach Formel (1)
epsilon <- rnorm(size, mean = 0, sd = sqrt(sigma_squared))
Yi <- beta_0 + beta_1 * Xi + epsilon

# Erstellen der Matrix X aus Formel (2)
X <- cbind(1, Xi)  # Die Spalten sind 1 und Xi

beta_hut <- solve(t(X) %*% X) %*% t(X) %*% Yi

beta_hut

###################################################
## Aufgabe 4 (b)
###################################################
plot(Xi, Yi, main = "Scatterplot mit Regressionsgeraden", xlab = "Xi", ylab = "Yi", pch = 19)

# Berechnete Regressionsgerade
abline(beta_hut[1], beta_hut[2], col = "red")

# Wahre Regressionsgerade
abline(beta_0, beta_1, col = "blue", lty = 2)

legend("topleft", legend = c("Berechnete Regressionsgerade", "Wahre Regressionsgerade"),
       col = c("red", "blue"), lty = c(1, 2), cex = 0.75)



###################################################
## Aufgabe 4 (c)
###################################################

set.seed(420)

n <- 50
X <- runif(n, 0, 2 * pi)
Y <- sin(X) + rnorm(n, mean = 0, sd = 0.1)
# Erstellen der Matrix X für Polynom der Ordnung 3
X_poly <- cbind(1, X, X^2, X^3)

beta_hut_poly <- solve(t(X_poly) %*% X_poly) %*% t(X_poly) %*% Y

plot(X, Y, main = "Scatterplot mit Regressionspolynom", xlab = "X", ylab = "Y", pch = 19)

# Regressionsgeraden und des Regressionspolynoms
curve(beta_hut_poly[1] + beta_hut_poly[2] * x + beta_hut_poly[3] * x^2 + beta_hut_poly[4] * x^3,
      add = TRUE, col = "red", lwd = 2)

legend("topright", legend = c("Regressionspolynom (d=3)"), col = c("red"), lty = 1, cex = 0.65)

