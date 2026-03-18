## Platon Gkotzamanidis
## Christóbal Marchado Krieger

###################################################
## Aufgabe 4 (a)
###################################################
rbinom_self <- function(n, size, prob){
  u <- runif(n, min = 0, max = 1)
  x <- qbinom(u, size = size, prob = prob) 
  return(x)
}
  
rnorm_self <- function(n, mean, sd){
  u <- runif(n, min = 0, max = 1)
  x <- qnorm(u, mean = mean, sd = sd)
  return(x)
}

###################################################
## Aufgabe 4 (b)
###################################################

# Generierung von binomialverteilten Zufallszahlen
set.seed(1)
rbinom_r <- rbinom(10000, size = 10, prob = 0.3)
rbinom_self <- rbinom_self(10000, size = 10, prob = 0.3)

# Generierung von normalverteilten Zufallszahlen
set.seed(1)
rnorm_r <- rnorm(10000, mean = 10, sd = 1)
rnorm_self <- rnorm_self(10000, mean = 10, sd = 1)

###################################################
## Aufgabe 4 (c)
###################################################

# Überprüfung der binomialverteilten Zufallszahlen
identical(rbinom_r, rbinom_self)  # Sollte TRUE ergeben, wenn die Zahlen übereinstimmen

# Überprüfung der normalverteilten Zufallszahlen
identical(rnorm_r, rnorm_self)  # Sollte TRUE ergeben, wenn die Zahlen übereinstimmen

#die Ergebnisse stimmen nicht gaz überein, denn 
#Die Ergebnisse sollten zwischen den integrierten R-Funktionen und Ihren eigenen Funktionen übereinstimmen,
#da beide Methoden die Inversionsmethode verwenden, um Zufallszahlen aus den entsprechenden Verteilungen zu generieren.

#Es kann jedoch geringfügige Unterschiede geben, 
#aufgrund der verschiedenen Algorithmen und Implementierungen,
#die in den integrierten R-Funktionen im Vergleich zu Ihren eigenen Funktionen verwendet werden.