library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

#-------------------------------------------------------------------------------------------------------------------------------------

#Aufgabe 1
#(i)   Erklären Sie den Unterschied zwischen einer numerischen Variable, einer Charakter-Variable und einem Faktor.
#(ii)  Warum ist das Faktorisieren von Charakter-Variablen für spätere Analysen wichtig?

#(i)  Eine Numerische Variable enthält zahlen, mit denen gerechnet werden kann (z.B 3, 10.5). Also sie repräsentiert quantitative Werte
#     Eine Character-Variable enthält Textdaten (Strings) (z.B. "Auto", "Deutschland"). R kann nicht damit rechnen.
#     Ein Faktor wird dazu genutzt, um mit kategorische Variablen zu arbeiten. Das heißt also, eine Faktor-Variable ist ein Datentyp, 
#     der Variablen die eine begrenzte Anzahl von möglichen Werten haben (z.B. Geschlecht, Wohnsituation, Beruf)
#     einer bekannten Mengen annehmen können.

#(ii) Das Faktorisieren von Character-Variablen ist für statistische Analysen besonders wichtig, da sie von statistische Modellen korrekt
#     verarbeitet werden können, was mit normalen Charakter-Variablen nicht immer der Fall ist und zu falschen Ergebnissen führen kann.

#-------------------------------------------------------------------------------------------------------------------------------------

#Aufgabe 2
#(i)  Erklären Sie, was ein Dataframe ist und was dessen Bestandteile sind. 
#(ii) Erklären Sie anhand des „Teenage Smartphone Addiction“- Datasets was ein Vektor/Variable, eine Beobachtung und ein Objekt ist.

#(i) Ein Dataframe in R ist eine zweidimensionale, tabellenähnliche Datenstruktur, die wie eine Tabelle in einer 
#    Tabellenkalkulation organisiert ist und aus Vektoren gleicher Länge besteht. Sie besteht aus einer geordneten Sammlung von Vektoren,
#    wobei die Vektoren alle die gleiche Länge haben müssen, aber unterschiedliche Datentypen aufweisen können. Man nennt die
#    Spalten und die Zeilen jeweils Variablen und Beobachtungen.


#(ii)
smartphone <- read.csv("teen.phone_addiction_dataset.csv")

#Eine Vektor/Variabe im Dataframe smartphone wäre zum Beispiel Daily_Usage_Hours, Age, Gender
#Eine Beobachtung ist eine Zeile im Dataframe smartphone, also die Daten eines/einer Jugendlichen
#Der gesamte Dataframe wird in R als ein Objekt gespeichert, also smartphone ist ein Objekt, das die komplette Tabelle enthält.    

#-------------------------------------------------------------------------------------------------------------------------------------

#Aufgabe 3 
#(i)   Laden Sie den Excel-Datensatz „Ausgaben“ in Ihr Environment (Sie benötigen hierfür das Paket readxl und die Funktion read_excel). 
#(ii)  Der Datensatz liegt Ihnen als wide-Format vor, transformieren Sie den Datensatz mit pivot_longer() in ein long-Format. 
#      Die Jahresspalten (Euro_2019 bis Euro_2024) sollen zu zwei Variablen zusammengefasst werden: eine Variable „Jahr“, mit den 
#      Jahreszahlen, und eine Variable „Euro“, die den Eurobetrag beinhaltet
#(iii) Erklären Sie worin sich die Datensätze je nach Format unterscheiden. Warum arbeiten wir in der Regel mit dem long-Format?

#(i)
ausgaben <- read_excel("einkaeufe_wideFormat.xlsx")

#(ii)
ausgaben_long <- ausgaben %>%
  pivot_longer(
    cols = Euro_2019:Euro_2024,
    names_to = "Jahr",
    values_to = "Euro"
  )

#(iii)
#In den Wide-Format ist jede Kategorie über mehrere Spalten verteilt (z.B. Euro_2019, Euro_2020, ...). 
#-> Gut zum Anschauen, schlecht für Analysen 

#In den Long-Format korrespondiert eine Spalte für eine Jahreszahl, und eine Spalte für den Wert (Euro)
#-> Daten stehen untereinander, statt nebeneinander

#Wir arbeiten in der regel mit den long-Format, denn viele wichtige Funktionen in R nur long-formatierte Dateien korrekt verarbeiten,
# z.B. group_by(), summarise(). Visualisierungen mit ggplot2 funktionieren im Allgemeinen auch besser im long-Format.

#-------------------------------------------------------------------------------------------------------------------------------------


#Aufgabe 4
#Ihre Variable Jahr enthält für jedes Jahr noch den Zusatz „Euro_“. 
#(i)   Erstellen Sie daraus eine Variable, die nur die Jahreszahl beinhaltet.
#      Hinweis: Nutzen Sie hierfür den Befehl as.integer() und gsub()
# (ii) Lassen Sie sich mit head() die ersten vier Zeilen Ihres Datensatzes ausgeben. 
#      Ihr Datensatz sollte nun vier Variablen beinhalten: ID, Art, Jahr und Euro.


#(i)

ausgaben_long <- ausgaben_long %>%
  mutate(
    Jahr = Jahr %>% 
      gsub("Euro_", "", .) %>% 
      as.integer()
  )

ausgaben_long %>% 
  head(4)

# 4 Einträge ?? Nicht 5 ? Was ist mit Euro_2018 ? 

#-------------------------------------------------------------------------------------------------------------------------------------


#Aufgabe 5 
#Runden Sie nun die Werte der Variablen Euro auf und berechnen, Sie die durchschnittliche Höhe an Ausgaben für Bücher pro Jahr.
#Hinweis: Sie benötigen hierfür u.a. die Befehle filter() und group_by(). Denken Sie an fehlende Werte!

ausgaben_buecher <- ausgaben_long %>%
  mutate(
    Euro = ceiling(Euro)   
  ) %>%
  filter(Art == "Bücher") %>%   
  group_by(Jahr) %>%          
  summarise(
    Durchschnitt = mean(Euro, na.rm = TRUE) 
  )

#-------------------------------------------------------------------------------------------------------------------------------------

#Aufgabe 6
#Faktorisieren Sie die Variable Jahr und Art.

ausgaben_long <- ausgaben_long %>%
  mutate(
    Jahr = factor(Jahr),
    Art  = factor(Art)
  )

#-------------------------------------------------------------------------------------------------------------------------------------

#Aufgabe 6
#Berechnen Sie für das Jahr 2023 die durchschnittlichen Ausgaben für jede Kategorie (Bücher, Kosmetik etc.)

ausgaben_2023 <- ausgaben_long %>%
  filter(Jahr == 2023) %>%      
  group_by(Art) %>%            
  summarise(
    Durchschnitt = mean(Euro, na.rm = TRUE)  
  )

#-------------------------------------------------------------------------------------------------------------------------------------

#Aufgabe 7
#(i)    Berechnen Sie nun die Ausgaben pro Kategorie pro Jahr. Im Anschluss daran:
#       Berechnen Sie den prozentualen Anteil jeder Kategorie an den Gesamtausgaben des jeweiligen Jahres.
#(ii)   Lassen Sie sich die prozentualen Anteile der Ausgaben je Kategorie und Jahr als gestapeltes Balkendiagramm ausgeben.
#           -  Der Plot soll die Überschrift „Prozentuale Ausgaben pro Kategorie und Jahr“ haben 
#           -  Beschriftung:
#                              x-Achse: Jahr
#                              y-Achse: Anteil der Ausgaben
#                              Legende: Kategorie

#(iii)  Lassen Sie sich die Daten als gruppiertes Balkendiagramm anzeigen.
#       Was ist der Vorteil eines gruppierten Balkendiagramms zu einem gestapelten Balkendiagramm?
#(iv)   Lassen Sie sich die prozentualen Anteile jeder Kategorie für die jeweiligen Jahre als faceted Plot anzeigen. 
#       Wichtig: Jeder Teilplot soll den Anteil an den Gesamtausgaben jeder Kategorie eines Jahres zeigen.

#(i)
ausgaben_pro_Kategorie_pro_Jahr <- ausgaben_long %>%
  group_by(Jahr, Art) %>%
  summarise(
    Summe = sum(Euro, na.rm = TRUE) 
  )

ausgaben_prozentualer_Anteil <- ausgaben_pro_Kategorie_pro_Jahr %>%
  group_by(Jahr) %>%
  mutate(
    Anteil = Summe / sum(Summe)
  )


#(ii)
ggplot(data = ausgaben_prozentualer_Anteil, aes(x = Jahr, y = Anteil, fill = Art)) +
  geom_col() +
  labs(
    x = "Jahr",
    y = "Anteil der Ausgaben",
    title = "Prozentuale Ausgaben pro Kategorie und Jahr",
    fill = "Kategorie"
  )

#(iii)
ggplot(ausgaben_prozentualer_Anteil, aes(x = Jahr, y = Anteil, fill = Art)) +
  geom_col(position = "dodge") +
  labs(
    title = "Prozentuale Ausgaben pro Kategorie und Jahr (gruppiert)",
    x = "Jahr",
    y = "Anteil der Ausgaben",
    fill = "Kategorie"
  )
#Vorteil: Man kann die Kategorien direkt nebeneinander vergleichen.
#Beispiel: Man sieht leichter ob Kosmetik oder Lebensmittel im Jahr 2022 mehr ausmacht, was im gestapelten Balkendiagramm
#schwierig zu erkennen ist.

#(iv)
ggplot(ausgaben_prozentualer_Anteil, aes(x = Jahr, y = Anteil, fill = Art)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Art) +
  labs(
    title = "Anteil der Ausgaben je Kategorie pro Jahr",
    x = "Jahr",
    y = "Anteil am Gesamtjahr"
  )
