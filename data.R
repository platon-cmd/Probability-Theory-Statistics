library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

#Sitzung 3
einkommen.w_csv <- read_csv("einkommen_w.csv")
einkommen.l_csv <- read_csv("einkommen_l.csv") #lieber so !!

einkommen.l <-  einkommen.w_csv %>%
  pivot_longer(cols = starts_with("Einkommen"), 
               names_to =  "Jahr",
               values_to = "Einkommen",
               names_prefix = "Einkommen_")


stud_sleep <- read_csv("student_sleeppatterns.csv")

View(stud_sleep)
nrow(stud_sleep)
ncol(stud_sleep)
names(stud_sleep) 
head(stud_sleep)
tail(stud_sleep)

is.na(stud_sleep)
anyNA(stud_sleep)

anyNA(stud_sleep$Gender) # speziell schauen bei der Gender spalte 



#faktorisieren von Variablen

stud_sleep_fz <- stud_sleep %>%      # mutate() überschreibt bestehende spalten
  mutate(Gender = as.factor(Gender),
         University_Year = as.factor(University_Year))

stud_sleep_fz  <- stud_sleep_fz %>%             # variablen umbenennen
  rename(wd_start = Weekend_Sleep_Start,
         wd_end = Weekend_Sleep_End)

stud_sleep_fz %>%                           #nur temporär gespeichert
  relocate(Sleep_Duration, Sleep_Quality)

stud_sleep_fz %>%
  select(-Age, -Sleep_Duration)

stud_sleep_fz %>%
  select(starts_with("A"))

stud_sleep_fz %>%
  select(ends_with("r"))



stud_sleep_fz %>%    
  arrange(Age)       #aufsteigend     

stud_sleep_fz %>%      
  select(Age, Caffeine_Intake) %>%
  arrange(desc(Age))   #absteigend



stud_sleep_fz %>%
  filter(Age == 23)

stud_sleep_fz %>% 
  filter(University_Year == "4th Year")

stud_sleep_fz %>%
  filter(Age >= 23 & Gender == "Female") %>% 
  select(Sleep_Duration, Age)

stud_sleep_fz_g <- stud_sleep_fz  %>% 
  filter(!(Gender == "Female"))


  
#Übung

  stud_sleep_fz %>%
  select(starts_with("Weekend"))

  stud_sleep_fz %>% 
    rename(Alter = Age, Uni_Year = University_Year)

  stud_sleep_fz %>% 
    select(Caffeine_Intake, University_Year) %>%
    arrange(University_Year)
  
  stud_sleep_fz %>%
    filter(Gender == "Female") %>%
    select(Student_ID, Physical_Activity, Sleep_Duration)

  stud_sleep_fz %>%
    filter(Gender == "Male" & Age >= 24)




