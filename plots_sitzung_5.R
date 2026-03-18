#################
### Sitzung 5 ###
#################

#Pakete laden

library(readr)
library(tidyr)
library(readxl)
library(dplyr)

#ggplot2 installieren und laden

install.packages("ggplot2")
library(ggplot2)

#Datensatz laden
#smartphone.addiction <- read.csv("C:/Users/ziegler/Desktop/SeminarWiSe25_Einführung in R/seminarR_ziegler/data/teen_phone_addiction_dataset.csv")

#Entfernen der Variablen 

#teenphone.addict_fz <- smartphone.addiction %>%
#select(c(- Name, -Location))

#Faktorisieren von Gender, School_Grade und Phone_Usage_Purpose

#teenphone.addict_fz <- teenphone.addict_fz %>%
# mutate(School_Grade = as.factor(School_Grade),
#  Phone_Usage_Purpose = as.factor(Phone_Usage_Purpose),
# Gender = as.factor(Gender))

#Aufrunden der Variablen 

#teenphone.addict_fz <- teenphone.addict_fz %>%
#mutate(across(c(Daily_Usage_Hours, Sleep_Hours, Exercise_Hours, Screen_Time_Before_Bed, Time_on_Social_Media, Time_on_Education,
#Weekend_Usage_Hours, Time_on_Gaming), ceiling))



#Zusammenhänge mit Scatterplots visualisieren


plot1 <- ggplot(data = teenphone.addict_fz, aes(x = Daily_Usage_Hours, y = Addiction_Level)) +
  geom_jitter() 

plot1

#Beschriftung x, y-Achsen mit labs(x = , y= )

#width = 0.5 ~ darf max. 0.5 Einheiten entlang der X-Achse verschoben werden
#size = 0.3 ~ Größe Punkte
#Alpha = 0.6 ~ Transparenz


plot1 <- ggplot(data = teenphone.addict_fz, aes(x = Daily_Usage_Hours, y = Addiction_Level)) +
  geom_jitter(width = 0.5, size = 3, alpha = 0.6, color = "purple") +
  labs(x = "Daily Smartphone Usage (in Hours)",
       y = "Level of Addiction")

plot1


#Skalen verändern: 2er Schritte mit scale_x_continuous() (0,12,2) und scale_y_continuous() (0,10,2)
#Für die Skalen die Range der Variablen beachten

plot1 <- ggplot(data = teenphone.addict_fz, aes(x = Daily_Usage_Hours, y = Addiction_Level)) +
  geom_jitter(width = 0.5, size = 3, alpha = 0.6, color = "purple") +
  labs(x = "Daily Smartphone Usage (in Hours)",
       y = "Level of Addiction") +
  scale_x_continuous(breaks = seq(0,12,2), #(from, to, by)
                     limits = c(0, 12)) +
  scale_y_continuous(breaks = seq(0,10,2), #(from, to, by)
                     limits = c(0, 10)) 


plot1





#Design: theme_minimal() statt Standard theme_gray()

plot1 <- ggplot(data = teenphone.addict_fz, aes(x = Daily_Usage_Hours, y = Addiction_Level)) +
  geom_jitter(width = 0.5, size = 3, alpha = 0.6, color = "purple") +
  labs(x = "Daily Smartphone Usage (in Hours)",
       y = "Level of Addiction") +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  scale_x_continuous(
    breaks = seq(0, 12, 2),   # x-Achse: 2er Schritte (from, to, by)
    limits = c(0, 12)
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),   # y-Achse: 2er Schritte
    limits = c(0, 10)
  ) +
  theme_minimal()

plot1














#Weitere Anpassungen am Design: Hintergrund und Rahmen anpassen

plot1 <- ggplot(data = teenphone.addict_fz, aes(x = Daily_Usage_Hours, y = Addiction_Level)) +
  geom_jitter(width = 0.5, size = 3, alpha = 0.6, color = "purple") +
  labs(x = "Daily Smartphone Usage (in Hours)",
       y = "Level of Addiction") +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  scale_x_continuous(
    breaks = seq(0, 12, 2),   # x-Achse: 2er Schritte (from, to, by)
    limits = c(0, 12)
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),   # y-Achse: 2er Schritte
    limits = c(0, 10)
  ) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    linewidth = 1))

plot1


#Regressionsgerade mit method = lm (linear model)


plot1 <- ggplot(data = teenphone.addict_fz, aes(x = Daily_Usage_Hours, y = Addiction_Level)) +
  geom_jitter(width = 0.5, size = 3, alpha = 0.6, color = "purple") +
  labs(x = "Daily Smartphone Usage (in Hours)",
       y = "Level of Addiction") +
  #
  scale_x_continuous(
    breaks = seq(0, 12, 2),   # x-Achse: 2er Schritte (from, to, by)
    limits = c(0, 12)
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),   # y-Achse: 2er Schritte
    limits = c(0, 10)
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),   
    panel.border = element_rect(          
      colour = "black",                 
      fill = NA,                          
      linewidth = 1                       
    )
    
  )


plot1







#Ergänzen von Variablen z.B. Parental Control

plot1 <- ggplot(data = teenphone.addict_fz, aes(x = Daily_Usage_Hours, y = Addiction_Level, 
                                                colour = Parental_Control)) +
  geom_jitter() +
  labs(x = "Daily Smartphone Usage (in Hours)",
       y = "Level of Addiction") +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  scale_x_continuous(
    breaks = seq(0, 12, 2),   # x-Achse: 2er Schritte (from, to, by)
    limits = c(0, 12)
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),   # y-Achse: 2er Schritte
    limits = c(0, 10)
  ) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    linewidth = 1))

plot1




table(teenphone.addict_fz$Parental_Control)




#Faktorisieren von Parental_Control, damit R Parental_Control als dichotome Variable mit den Ausprägungen 0 und 1 erkennt
#Beschriftung der Legende

teenphone.addict_fz <- teenphone.addict_fz %>%
  mutate(Parental_Control = as.factor(Parental_Control))

plot1 <- ggplot(data = teenphone.addict_fz, aes(x = Daily_Usage_Hours, y = Addiction_Level, 
                                                colour = Parental_Control)) +
  geom_jitter(width = 0.5, size = 3, alpha = 0.7) +
  labs(x = "Daily Smartphone Usage (in Hours)",
       y = "Level of Addiction",
       colour = "Parental Control") +
  scale_x_continuous(
    breaks = seq(0, 12, 2),   # x-Achse: 2er Schritte (from, to, by)
    limits = c(0, 12)
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),   # y-Achse: 2er Schritte
    limits = c(0, 10)
  ) +
  scale_colour_manual(values = c("0" = "seagreen", "1" = "blueviolet"),
                      labels = c("0" = "No", "1" = "Yes"))

plot1

