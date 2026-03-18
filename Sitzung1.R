#Übung 1
x<-21
y<-23
x_plus_y <- x+y
x_minus_y <- x-y
z <- x_plus_y/2

#String Variable
Hunderassen <- c("Golden Retriever", "Dackel", "Labrador")
Hunderassen


Dessert <- c(Kuchen, Eis, Obst) #----> Fehler (ohne "")
  

#Numerische Variable 
Noten <- c(1.5,2,5,2.5)


#logische_Variable
Hunderassen == "Dackel"



#Übung 2
#(a)
Numerische_Variable <- c(0,1,2,3,4,5,6,7,8,9)

#(b)
setdiff(Numerische_Variable, 2)  # Deleting the 2,  alternativ : ---> Numerische_Variable[!Numerische_Variable == 3] #deleting the 3
#alternativ numerische_variable <- numerische_variable[-3] ---> löscht die 3- stelle des arrays

#(c)
farben <- c("grün", "rot", "gelb", "blau")
farben_factor <- as.factor(farben) #umwandlung in Faktor-Variable

#(d)
class(farben)
class(Numerische_Variable)
length(farben)
length(Numerische_Variable)
is.na(farben)
is.na((Numerische_Variable))

getwd()
