library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

stud_sleep <- read_csv("student_sleeppatterns.csv")
stud_sleep_fz <- stud_sleep %>%
  mutate(
    Gender = as.factor(Gender),
    University_Year = as.factor(University_Year)
  )

stud_sleep_fz <- stud_sleep_fz %>%
  mutate(
    across(c(Study_Hours, Sleep_Duration, Screen_Time), ceiling)
    )
View(stud_sleep_fz)
         

stud_sleep_fz <- stud_sleep_fz %>%
  mutate(Birthyear = 2022- Age) %>%
  relocate(Birthyear, .before = Gender)

View(stud_sleep_fz)

stud_sleep_fz <- stud_sleep_fz %>% 
  mutate(wake.up_Difference = Weekend_Sleep_End - Weekday_Sleep_End)
View(stud_sleep_fz)

stud_sleep_fz <- stud_sleep_fz %>%
  mutate(
    Study_Hours_Group = case_when(
      Study_Hours <= 4 ~ "low",
      Study_Hours > 4 & Study_Hours <= 8 ~ "medium",
      Study_Hours > 8 ~ "high"
    )
  ) %>%
  relocate(Study_Hours_Group, .after = Study_Hours)

stud_sleep_fz %>%
  count(Study_Hours_Group, sort = TRUE)

stud_sleep_fz <- stud_sleep_fz %>%
  mutate(
    Sleep_Quality_Group = case_when(
      Sleep_Quality <= 3 ~ "low",
      Sleep_Quality >= 4 & Sleep_Quality <= 7 ~ "medium",
      Sleep_Quality >= 8 ~ "high"
    )
  ) %>%
  relocate(Sleep_Quality_Group, .after = Sleep_Quality)

stud_sleep_fz %>%
  count(Sleep_Quality_Group, sort = TRUE)
View(stud_sleep_fz)

stud_sleep_fz %>%
  summarise(mean(Sleep_Quality, na.rm = TRUE))

stud_sleep_fz %>%
  summarise(sd(Sleep_Quality, na.rm = TRUE))

stud_sleep_sum <- stud_sleep_fz %>%
  group_by(Sleep_Quality_Group) %>%
  summarise(avg_sleep_duration = mean(Sleep_Duration, na.rm = TRUE) )
View(stud_sleep_sum)


stud_sleep_sum2 <- stud_sleep_fz %>%
  group_by(Gender) %>%
  summarise(avg_sleep_duration = mean(Sleep_Duration, na.rm = TRUE),
            avg_study_hours = mean(Study_Hours, na.rm = TRUE))
View(stud_sleep_sum2)

stud_sleep_mutate <- stud_sleep_fz %>%
  group_by(Sleep_Quality_Group) %>%
  mutate(
    avg_sleep_duration = mean(Sleep_Duration, na.rm = TRUE)
  )
View(stud_sleep_mutate)


is_grouped_df(stud_sleep_fz)

is_grouped_df(stud_sleep_mutate)

stud_sleep_mutate <- stud_sleep_mutate %>%
  ungroup()


stud_sleep_fz %>%
  count(Gender, sort = TRUE)


stud_sleep_fz %>%
  group_by(Gender) %>%
  summarise(propotion = n()/nrow(stud_sleep_fz)*100)




#Aufgaben 

teen.phone_addiction_dataset <- read_csv("teen.phone_addiction_dataset.csv")
View(teen.phone_addiction_dataset)

dataset_removed <- teen.phone_addiction_dataset %>%
  select(-Name, -Location)

dataset_removed <- dataset_removed %>%
  mutate(
    Gender = as.factor(Gender),
    School_Grade = as.factor(School_Grade),
    Phone_Usage_Purpose = as.factor(Phone_Usage_Purpose)
  )

dataset_removed <- dataset_removed %>%
  mutate(
    across(c(Daily_Usage_Hours, Sleep_Hours, 
             Exercise_Hours, 
             Screen_Time_Before_Bed, 
             Time_on_Social_Media, 
             Time_on_Education, 
             Weekend_Usage_Hours), ceiling)
  )
View(dataset_removed)

dataset_removed %>%
  filter(Sleep_Hours < 7) %>%
  select(Depression_Level)

dataset_removed <- dataset_removed %>%
  count(Depression_Level, sort = TRUE)

dataset_removed <- dataset_removed %>%
  group_by(Depression_Level_group) %>%
  summarise(
    avg_addict_level = round(mean(Addiction_Level, na.rm = TRUE), 1)
  )

dataset_removed <- dataset_removed %>%
  group_by(Gender) %>%
  summarise(
    avg_addict_level = round(ceiling(Addiction_Level, na.rm = TRUE), 1)
  )

dataset_removed <- dataset_removed %>%
  group_by(Gender) %>%
  summarise(
    avg_addict_level = ceiling(Addiction_Level, na.rm = TRUE)) %>%
  mutate(avg_addict_level = round(avg_addict_level, 1))

usage_summary_gender <- teen.phone_addiction_dataset &>&
  group_by(Gender) %>%
  summarise(mean_usage = mean(Daily_Usage_Hours, na.rm = TRUE))

plot <- ggplot(usage_summary_gender)


#Sitzung 5, plotten 


plot1 <- ggplot(data = dataset_removed, aes(x = Daily_Usage_hours, y= Addict_Level)) +
geom_jitter()
  
einkaeufe <- read_csv("Einkaeufe.csv")
plot <- ggplot(einkaeufe)
