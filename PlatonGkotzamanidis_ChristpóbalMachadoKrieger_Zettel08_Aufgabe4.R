## Platon Gkotzamanidis  
## Christóbal Marchado Krieger 

###################################################
## Aufgabe 4 (a)
###################################################

## (i)
set.seed(2024)
n <- 1000
sigma <- 1
X <- rnorm(n, mean = 0, sd = sigma)
Y <- sample(c(-1, 1), n, replace = TRUE)

Z <- X * Y

plot(X, Z, main = "Scatterplot von (X, Z)", xlab = "X", ylab = "Z", pch = 16, col = rgb(0, 0, 1, 0.5))


## (ii)
set.seed(2024)
n <- 1000
tilde_X <- rnorm(n, mean = 0, sd = 1)
tilde_Z <- rnorm(n, mean = 0, sd = 1)

plot(tilde_X, tilde_Z, main = "Scatterplot von (tilde_X, tilde_Z)", xlab = "tilde_X", 
     ylab = "tilde_Z", pch = 16, col = rgb(1, 0, 0, 0.5))


###################################################
## Aufgabe 4 (b)
###################################################

set.seed(2024)

plot_hist_Tn <- function(n, lambda = 1, num_sim = 1000) {
  T_n <- numeric(num_sim)
  
  for (i in 1:num_sim) {
    X <- rpois(n, lambda)
    X_bar <- mean(X)
    T_n[i] <- sqrt(n) * (X_bar - lambda) / sqrt(lambda)
  }
  
  hist(T_n, breaks = 30, probability = TRUE, main = paste("Histogramm der T_n-Werte für n =", n), 
       xlab = "T_n", col = "lightblue", border ="black")
  curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red", lwd = 2)
}

par(mfrow = c(2, 2))  
plot_hist_Tn(10)
plot_hist_Tn(50)
plot_hist_Tn(100)
plot_hist_Tn(500)

