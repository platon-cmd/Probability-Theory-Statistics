## Platon Gkotzamanidis  
## Christóbal Marchado Krieger 

###################################################
## Aufgabe 5 (a)
###################################################

set.seed(420)
n <- 1000
mu = 2
sigma = 1
x <- rnorm(n, mean = mu, sd = sigma)

exp_mu_hat <- exp(mean(x))
cat("Schätzer für exp(µ):", exp_mu_hat, "\n")

exp_mu_true <- exp(mu)
cat("Tatsächlicher Wert von exp(2):", exp_mu_true, "\n")

num_simulations <- 10000
exp_mu_hats <- numeric(num_simulations)

for (i in 1:num_simulations) {
  x <- rnorm(n, mean = mu, sd = sigma)
  exp_mu_hats[i] <- exp(mean(x))
}

hist(exp_mu_hats, breaks = 50, probability = TRUE, main = "Histogramm der Schätzer für exp(µ)",
     xlab = expression(hat(exp(mu))), col = "lightblue")

abline(v = exp_mu_true, col = "red", lwd = 2)


###################################################
## Aufgabe 5 (b)
###################################################

mu <- 2
sigma <- 1

x <- rnorm(n, mean = mu, sd = sigma)

E_exp_X1_empirical <- mean(exp(x))
exp_E_X1_empirical <- exp(mean(x))

cat("Empirischer Wert von E[exp(X1)]:", E_exp_X1_empirical, "\n")
cat("Empirischer Wert von exp(E[X1]):", exp_E_X1_empirical, "\n")

E_exp_X1_theoretical <- exp(mu + sigma^2 / 2)
exp_E_X1_theoretical <- exp(mu)

cat("Theoretischer Wert von E[exp(X1)]:", E_exp_X1_theoretical, "\n")
cat("Theoretischer Wert von exp(E[X1]):", exp_E_X1_theoretical, "\n")

if (abs(E_exp_X1_empirical - exp_E_X1_empirical) > 1e-5) {
  cat("E[exp(X1)] ist nicht gleich exp(E[X1]).\n")
} else {
  cat("E[exp(X1)] ist gleich exp(E[X1]).\n")
}

