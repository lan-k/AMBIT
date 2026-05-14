## read in the AMBIT data and create the analysis dataset
rm(list=ls())
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(janitor)


select = dplyr::select
study_start = as.Date("2023-06-01")
dat <- read.csv("../AMBIT.csv", stringsAsFactors = F, 
                na.strings = c("","N/A", "N/S")) %>%
  janitor::clean_names()

time1 = dat %>% 
  select(study_id, dob, sex, cald_y_n, indigenous_y_n,
         starts_with("time_1")) %>%
  mutate(time = "t1")

names(time1) <- sub('^time_1_', '', names(time1))
time1 = time1 %>% rename(clozapine_y_n = cloz_y_n)

time2 = dat %>%
  select(study_id, dob, sex, cald_y_n, indigenous_y_n,
         starts_with("time_2")) %>%
  mutate(time = "t2")

names(time2) <- sub('^time_2_', '', names(time2))


ambit = bind_rows(time1, time2) %>% filter(!is.na(study_id))

miss = ambit %>% filter(is.na(days_in_hospital))

names(ambit) = sub("_y_n","", names(ambit))

names(ambit)


#exclude patients with missing data in key outcomes
miss = ambit %>%
  filter(is.na(days_in_hospital), is.na(episode_1_date_opened),
         is.na(primary_diagnosis), is.na(ed_presentations),
         is.na(ncmht_contacts))

length(unique(miss$study_id)) #121
table(miss$time) #59 in t1 and 62 in t2


#remove patients with all missing data
ambit = anti_join(ambit, miss)

sum(is.na(ambit$primary_diagnosis))  #25

##fix dates
ambit = ambit %>%
  mutate(across(matches("date|dob"), ~as.Date(., format = "%d/%m/%Y")),
         age = floor(as.numeric(study_start - dob)/365))



#fix missing hospital days & admissions

ambit = ambit %>%
  mutate(days_in_hospital = case_when(!is.na(days_in_hospital) ~ days_in_hospital,
                                      is.na(days_in_hospital) & is.na(episode_1_date_opened) ~ 0,
                                      TRUE ~ as.numeric(episode_1_date_closed - episode_1_date_opened)),
         hospital_admissions = case_when(is.na(hospital_admissions) & days_in_hospital == 0 ~ 0,
                                         is.na(hospital_admissions) & days_in_hospital > 0 ~ 1, #manually checked
                                         TRUE ~ hospital_admissions),
         ed_presentations = ifelse(is.na(ed_presentations), 0, ed_presentations))


table(ambit$time) #t1 114 t2 111
hist(ambit$days_in_hospital)
summary(ambit$days_in_hospital) #was 4 missing, now none
summary(ambit$ed_presentations) #was 4 missing, now none
summary(ambit$hospital_admissions) #was 4 missing, now none

tapply(ambit$days_in_hospital, ambit$time, summary)
tapply(ambit$ed_presentations, ambit$time, summary)
tapply(ambit$ncmht_contacts, ambit$time, summary)

##create binary variables
ambit = ambit %>%
  mutate(admitted = as.numeric(days_in_hospital > 0),
         ed = as.numeric(ed_presentations > 0),
         sex = ifelse(sex == "Another Term", "Other", sex))


miss2 = ambit %>% filter(is.na(primary_diagnosis))
#check diagnoses
table(ambit$primary_diagnosis, useNA = "always")

diag1 = ambit %>% rename(diagnosis = primary_diagnosis) %>% 
  group_by(diagnosis) %>% summarise(n=n())

diag2 = ambit %>% rename(diagnosis = secondary_diagnosis) %>% 
  group_by(diagnosis) %>% summarise(n=n())

diag3 = ambit %>% rename(diagnosis = third_diagnosis) %>% 
  group_by(diagnosis) %>% summarise(n=n())

diag4 = ambit %>% rename(diagnosis = fourth_diagnosis) %>% 
  group_by(diagnosis) %>% summarise(n=n())

diag= bind_rows(diag1, diag2, diag3, diag4) %>%
  group_by(diagnosis) %>%
  summarise(n=sum(n, na.rm=T))
  

knitr::kable(diag)


save(ambit, file = "../ambit.rds")
