## Zettel 1 Aufgabe 4
## Platon Gkotzamanidis
## Christóbal Machado Krieger

## 4a)
set.seed(2024)
Krank <- rep("K", 20)
Gesund <- rep("G", 40)
omega <- c(Krank, Gesund)

## 4b)
sample(omega, 10, replace = FALSE, prob = NULL)
 # bei jeder Ausführung immer verschiedene Ergebnisse für die Anzahl der kranken Mäusen
 # 1. Ausführung: 3 Krank
 # 2. Ausführung: 2 Krank
 # 3. Ausführung: 4 Krank

## 4c)
WDH <- 10000 #wie oft wird das Experiment durchgeführt                                      
O <- c("K", "K", "K", "K", "K", "K", "K", "K", "K", "K", 
      "K", "K", "K", "K", "K", "K", "K", "K", "K", "K", 
      "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", 
      "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", 
      "G", "G", "G", "G", "G", "G", "G", "G", "G", "G", 
      "G", "G", "G", "G", "G", "G", "G", "G", "G", "G") #Liste von den 60 labormäuse, davon 20 Krank, 40 gesund
K_rep <- rep(0, WDH) #erstelle eine Liste mit WDH-viele einträge und setze alle Einträge = 0
for(i in 1:WDH){ #erstelle eine Schleife (jede Wiederholung setzt den i-ten Wert der K_rep Liste einen Wert zu)
  Auswahl <- sample(O, 10) #Führe das experiment durch, ziehe 10 Labormäuse aus Liste O
  Auswahl_K <- Auswahl[Auswahl == "K"] #erstelle einen Vektor mit den kranken Labormäusen, aus den 10 gezogenen
  Auswahl_G <- Auswahl[Auswahl == "G"] #erstelle einen Vektor mit den gesunden Labormäusen, aus den 10 gezogenen
  Anzahl_K <- length(Auswahl_K) #Anzahl der kranken Labormäusen aus den 10 gezogenen
  Anzahl_G <- length(Auswahl_G) # Anzahl der gesunden Labormäusen aus den 10 gezogenen
  K_rep[i] <- Anzahl_K #setze in der Liste, mit WDH viele Einträgen, den i-ten Eintrag gleich der Anzahl der kranken Mäusen aus den Experiment
} #Wiederhole das Experiment WDH-mal, und setze immer den nächsten Antrag in der Liste mit der Anzahl der kranken Mäusen
length(K_rep[K_rep == 3])/WDH 
#length(K_rep[K_rep == 3]) = #{i in K_rep : i = 3}
# =>length(K_rep[K_rep == 3])/WDH ist das Verhältnis von {wie oft werden genau 3 kranke Mäuse gezogen} und {ziehe WDH-mal zufällig 10 Mäuse}


#(i): Der Code führt das Zufallexperiment aus Aufgabe2 WHD-mal durch und berechnet das Verhältnis von 
     # [wie oft werden genau 3 kranke Mäuse gezogen] und [ziehe WDH-mal zufällig 10 Mäuse]

#(ii): WDH=1 => length(K_rep[K_rep == 3])/WDH = 1 
      #WDH=10 => length(K_rep[K_rep == 3])/WDH = 0.3 
      #WDH=100 => length(K_rep[K_rep == 3])/WDH = 0.23
      #WDH=1000 => length(K_rep[K_rep == 3])/WDH = 0.293 ; 
      #WDH=10000 => length(K_rep[K_rep == 3])/WDH = 0.2855 ; 
#für WDH -> ∞ nährt sich die Zahl length(K_rep[K_rep == 3])/WDH die Wahrscheinlichkeit, dass von 10 ausgewählten Mäusen genau 3 krank sind

#(iii):
hist(K_rep)
#Wir sehen, dass wenn 10 Mäuse zufällig ausgewählt werden, meistens genau 3 davon krank sind (Maximum).

