##  Platon Gkotzamanidis  
## Christóbal Marchado Krieger 

###################################################
## Aufgabe 4 (a)
###################################################
set.seed(2024)
WDH <- 1000
p = 0.5
samples <- rbinom(WDH, 1, p)
partial <- rep(0, WDH)

for (n in 1:WDH) {
  result <- 0
  for (i in 1:n) {
    result <- result + samples[i]
  }
  partial[n] <- (1/n)*result
}

plot(partial,
     xlab = "n", 
     ylab = "S_n", 
     xlim = c(0, WDH), 
     ylim = c(0.4,1),)
abline(h=p, col="red")
abline(h=p-0.01, col="blue", lty ="dashed")
abline(h=p+0.01, col="blue", lty ="dashed")
#Die Werte nähern sich die rote linie an.
###################################################
## Aufgabe 4 (b)
###################################################
set.seed(2024)
WDH <- 100
p = 0.5
samples <- rbinom(WDH, 1, p)
partial <- rep(0, WDH)

for (n in 1:WDH) {
  result <- 0
  for (i in 1:n) {
    result <- result + samples[i]
  }
  partial[n] <- (1/n)*result
}

plot(partial,
     type = "l", 
     xlab = "n", 
     ylab = "S_n", 
     xlim = c(0, WDH), 
     ylim = c(0,1),
     col = rgb(0, 0, 0, alpha = 0.1))
abline(h=p, col="red")
abline(h=p-0.01, col="blue", lty ="dashed")
abline(h=p+0.01, col="blue", lty ="dashed")

###################################################
## Aufgabe 4 (c)
# p nah der 1     => alle Werte verammeln sich zwischen den 2 blau gestrichelten Linien (nah an der 1)
# p nah an der 0  => alle Werte verammeln sich zwischen den 2 blau gestrichelten Linien (nah an der 0)
###################################################

