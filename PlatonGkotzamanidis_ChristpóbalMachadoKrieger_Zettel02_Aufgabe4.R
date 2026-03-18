
#(a)
set.seed(420)
WDH <- 1000
a <- rep(0, WDH)
for (i in 1:WDH) {
  Tueren <- c("G", "Z_1", "Z_2")
  Ziegen <- c("Z_1", "Z_2") 
  gezogen <- sample(Tueren, 1, replace = FALSE, prob = NULL)
  Tueren <- Tueren[which(Tueren != gezogen)]
  Ziegen <- Ziegen[which(Ziegen != gezogen)]
  
  Moderator <- sample(Ziegen, 1, replace = FALSE, prob = NULL)
  Tueren <- Tueren[which(Tueren != Moderator)]
  
  wechseln <- Tueren[which(Tueren != gezogen & Tueren != Moderator)]
  if(gezogen == "G"){a[i] <- "Verloren"}
  if(gezogen != "G"){a[i] <- "Gewonnen"}
}

length(a[which(a == "Gewonnen")])/WDH

#(c)

set.seed (420)
n = 20
lambda <- 10
p = lambda/n
WDH <- 10000
## Hier Ihr Code
samples <- dbinom(0:20, n, p)
Unterste <- 0 
Oberste <- 30
xx <- seq(from = Unterste , to = Oberste , by = 1)


plot(samples, xlim = c(Unterste, Oberste), type = "h", col = "red", lwd = 10,
     , xlab = "k", ylab = "P(X)=k", main ="Poisson-Approximation")
lines(dpois(0:20, lambda), col = "blue", pch = 15)
legend("topleft", c("Poisson", "Binomial"), fill = c("blue", "red", ""))



