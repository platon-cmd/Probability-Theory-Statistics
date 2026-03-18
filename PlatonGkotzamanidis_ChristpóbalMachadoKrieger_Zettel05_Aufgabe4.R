##  Platon Gkotzamanidis  
## Christóbal Marchado Krieger 

###################################################
## Aufgabe 4 (a)
###################################################
set.seed(420)
n_max <- 10000
X <- rbinom(n_max, 10, 0.2)
n <- 1:n_max
Xbar <- cumsum(X) / n
sigma2 <- cumsum((X - Xbar)^2) / n

plot(n, Xbar, type = "l", xlab = "n", ylab = "Mittelwert und Varianz", ylim = c(0,3.5))
lines(n, sigma2, col = "red")
abline(h = 2, lty = 2)
abline(h = 1.6, col = "red", lty = 2)
legend("topright", legend = c("Mittelwert", "Varianz", "E[X]", "Var(X)"), 
       col = c("black", "red", "black", "red"), lty = c(1, 1, 2, 2))

#Für den Mittelwert Xbar_n erwarten wir, dass er sich mit wachsendem n dem theoretischen 
#Erwartungswert E[X] = 10 * 0.2 = 2 annähert, gemäß dem schwachen Gesetz der großen Zahl.

#Für die Varianz σ^2_n erwarten wir, dass sie sich mit wachsendem n der theoretischen 
#Varianz Var(X) = 10 * 0.2 * (1 - 0.2) = 1.6 annähert, ebenfalls aufgrund des schwachen 
#Gesetzes der großen Zahl.


###################################################
## Aufgabe 4 (b)
###################################################
x <- seq(-5, 5, length.out = 100)
plot(x, dcauchy(x, 0, 1), type = "l", xlab = "x", ylab = "Dichte")
#Die Cauchy-Verteilung hat keinen endlichen Erwartungswert, da die Integrale für den 
#Erwartungswert divergieren. Daher würden wir für eine Cauchy-verteilte Zufallsvariable 
#keinen endlichen Erwartungswert erwarten.


###################################################
## Aufgabe 4 (c)
###################################################
set.seed(420)
n_max <- 10000
Y <- rcauchy(n_max, 0, 1)
n <- 1:n_max
Ybar <- cumsum(Y) / n

plot(n, Ybar, type = "l", xlab = "n", ylab = "Mittelwert")
abline(h = 0, lty = 2)
#Da die Cauchy-Verteilung keinen endlichen Erwartungswert hat, konvergiert der 
#Stichprobenmittelwert Ybar_n nicht gegen einen endlichen Wert, sondern "schwankt" um 0 
#herum, ohne dass ein klares Muster erkennbar ist. Dies liegt daran, dass das schwache 
#Gesetz der großen Zahl für die Cauchy-Verteilung nicht gilt.

