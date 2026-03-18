
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
library(dplyr)
library(tidyr)
library(readr)
#Übung 1

#a)
y <- c(1,2,3,4,5,6,7,8,9,10)
mean(y)
var(y)
min(y)
max(y)
median(y)


#b)
x <- c(0.6,3.5,7,1.8,NA, 2)
mean(x, na.rm = FALSE) 
mean(x, na.rm = TRUE) 


#verschachtelung

x1 <- round(mean(c(4.56,8.99,11.03,3,34,1.55)), digits = 1)

#pipe operator

x2 <- c(4.56, 8.99, 2.33, 1.55) %>% mean() %>% round(digits = 1)



#Übung 2

#a)
x <- c(4.56,8.99,11.03,3.34,1.55)
y <- round(mean(x), digits = 1 )

y1 <-  c(4.56,8.99,11.03,3.34,1.55) %>%
  mean() %>%
  round(digits = 1)


#Dataframes

x <- c("platon", "nikoleta", "Max")
y<- c(23,24,40)
z <- c("Mathe", "Sozio", "Physik")
w <- c("Haselnuss", "Haselnuss", "Schokolade")

df <- data.frame(x,y,z,w)

x1 <- c("platon", "nikoleta", "Max")
y1<- c(23,24,40)
z1 <- c("Mathe", "Sozio", "Physik")
w1 <- c("Haselnuss", "Haselnuss", "Schokolade")



df_1 <- data.frame(x1,y1,z1,w1)


df_merge <-  merge(df, df_1)





