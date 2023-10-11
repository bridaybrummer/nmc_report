# COmverted code from stat do file from chatgpt.
library(dplyr)
library(readxl)
library(tidyverse)
library(janitor)
library(grates)
library(ISOweek)
library(lubridate)
library(data.table)

#(tibble)
#stata2script(tibble)


#tibble[c(1:100), ]
#stata2script(excel_load_1, verbose = T)
stata2script<- function(excel_load, verbose = T){
  
NMC <- excel_load
#nrow(NMC)
#names(NMC)
# Check Notifiation_date by quarter and month
#xtabs(~as_quarter(NMC$Notification_Date))
#xtabs(~as_month(NMC$Notification_Date))
# Check Notifiation_date by quarter and month
#xtabs(~as_quarter(NMC$Diagnosis_Date))
#xtabs(~as_month(NMC$Diagnosis_Date))

#xtabs(~ is.na(Diagnosis_Date)+ Case_Type, NMC)

#table(NMC$Province)
# Exclusion of discarded, quality assurance cases, foreign cases

data <- NMC %>%
  mutate(exceptions = 0)  

names(data) <- str_to_lower(names(data))

# Check for discarded cases
xtabs( ~ epidemiological_classification, data = data)

data1 <- data %>%
  mutate(exceptions = if_else(epidemiological_classification == "Discarded case", 1, exceptions))

# Check and drop discarded cases
table(data1$epidemiological_classification)

data2 <- data1 %>%
  filter(epidemiological_classification != "Discarded case")

nrow(data1)
nrow(data2)

# Check for category 4 conditions
table(data2$condition)
unique(data2$condition)

# No cases

# Check provinces
table(data2$province, useNA = "always")

data2 <- data2 %>%
  arrange(province)

data2 %>%
  filter(province == "UNKNOWN") %>%select(facility)

xtabs(~ facility, data = data2%>%
        filter(province == "UNKNOWN") )
  #View()  # Manually inspect and handle "UNKNOWN" province cases

# Handle other facility exceptions
data2 <- data2 %>%
  mutate(exceptions = if_else(facility == "Rcpa Quality Assurance" | facility == "Uk Neqas Quality Assurance" |  facility == "qcmd quality assurance", 1, exceptions))

data3 <- data2 %>%
  filter(facility != "Uk Neqas Quality Assurance")%>%
  filter(!grepl("qcmd quality assurance", ignore.case = T, facility))

data3%>%filter(facility == "Qcmd quality assurance")

# Drop other facilities
data4 <- data3 %>%
  filter(facility != "Namibia Institute Of Pathology")
  

# Replace province for specific facilities
data5 <- data4 %>%
  mutate(province = if_else(facility == "Wits Rhi Sex Worker Program Wc" | facility == "Wits Rhi Agyw Prep Program Wc", "WC Western Cape", province))%>%
  mutate(province = case_when((case_id == "230627_43520481") ~ "LP Limpopo", .default = province))

data4%>%filter(case_id == "230627_43520481")%>%view()


xtabs(~is.na(data5$province))
# Check for studies (facility "nicd")
data5 %>%
  filter(facility_type == "STUDY")%>%
 View()  # Manually inspect and handle study cases
# These 

data5 %>%
  filter(grepl("Nicd", facility))# %>%
#  View()  # Manually inspect and handle cases with "Nicd" in the facility

# Remaining unknown facility is a CS from lab
data5 <- data5 %>%
  arrange(patient_name)

#data5 %>%
#  filter(grepl("SAMPLE", patient_name)) %>%
#  View()  # Manually inspect and handle cases with "SAMPLE" in the patient_name

data6 <- data5 %>%
  filter(!grepl("SAMPLE", patient_name))

#data6 %>%
#  filter(grepl("SAMPLE", patient_surname)) %>%
#  View()  # Manually inspect and handle cases with "SAMPLE" in the patient_surname

data7 <- data6 %>%
  filter(!grepl("SAMPLE", patient_surname))

#data7 %>%
#  filter(grepl("SURVEY", patient_name)) %>%
#  View()  # Manually inspect and handle cases with "SURVEY" in the patient_name

data8 <- data7 %>%
  filter(!grepl("SURVEY", patient_name))

#data8 %>%
#  filter(grepl("SURVEY", patient_surname)) %>%
#  View()  # Manually inspect and handle cases with "SURVEY" in the patient_surname

data9 <- data8 %>%
  filter(!grepl("SURVEY", patient_surname))

#data9 %>%
#  filter(grepl("Biorad Hiv/heps", patient_surname))# %>%
#  View()  # Manually inspect and handle cases with "Biorad Hiv/heps" in the patient_surname

data10 <- data9 %>%
  filter(!grepl("Biorad Hiv/heps", patient_surname))
nrow(data)
nrow(data10)
# Drop specific Cholera cases
data11 <- data10 %>%
  filter(!(condition == "Cholera" & case_id == "230301_4184306")) %>%
  filter(!(condition == "Cholera" & case_id == "230309_41996741")) %>%
  filter(!(condition == "Cholera" & case_id == "230302_41850801")) %>%
  filter(!(condition == "Cholera" & case_id == "230315_4204179")) %>%
  filter(!(condition == "Cholera" & case_id == "230315_4204180")) %>%
  filter(!(condition == "Cholera" & case_id == "230315_4204181")) %>%
  filter(!(condition == "Cholera" & case_id == "230315_4204182")) %>%
  filter(!(condition == "Cholera" & case_id == "230331_42235361"))


nrow(data11)
data_semcleaned <- data

# ALTERNATIVE WAY
#data <- data %>%
#  mutate(QA = if_else(nmcfacility == "RCPA QUALITY ASSURANCE" | nmcfacility == "UK NEQAS QUALITY ASSURANCE", 1, 0)) %>%
#  mutate(province = case_when(
#    facility == "WITS RHI SEX WORKER PROGRAM WC" ~ "WC Western Cape",
#    facility == "WITS RHI AGYW PREP PROGRAM WC" ~ "WC Western Cape",
##    TRUE ~ province
#  )) %>%
#  mutate(QA = if_else(
#    str_detect(patient_name, "SAMPLE") | str_detect(patient_surname, "SAMPLE") |
#      str_detect(patient_surname, "SURVEY") | str_detect(patient_surname, "Biorad Hiv/heps"),
#    1, QA
#  )) %>%
#  drop_na(QA)

##################################################################

# Clerically review
data12 <- data11 %>%
  filter(patient_name != "UNKNOWN") %>%
  filter(patient_surname != "UNKNOWN")

# Clean string variables
data13 <- data12 %>%
  mutate(patient_name = str_replace_all(patient_name, "[-:.;/()\\[\\],' ]", "")) %>%
  mutate(patient_surname = str_replace_all(patient_surname, "[-:.;/()\\[\\],]", ""))

# Clean DOB variable
data14 <- data13 #%>%
  #mutate(patient_dob = as.Date(as.character(patient_dob), format = "%d%b%Y")) #%>%
  #mutate(patient_dob = if_else(as.numeric(as_year(patient_dob)) == 1923, "NA", as_date(patient_dob))) %>%
  #mutate(patient_dob = if_else(month(patient_dob) == 1 & day(patient_dob) == 1 & year(patient_dob) == 1900, NA_Date_, patient_dob))

xtabs(~data14$patient_age + data14$patient_age_unit) 

data14.1 <- data14%>%mutate(
  Age_years = case_when(patient_age_unit == "Days"~ as.numeric(floor(as.numeric(patient_age)/365.25)),
                        patient_age_unit == "Months"~ as.numeric(floor(as.numeric(patient_age)/12)),
                        patient_age_unit == "Years"~ as.numeric(floor(as.numeric(patient_age)))
  )
)


data14.2 <- data14.1%>% mutate(
  age_dob = floor(
    (
    difftime( as_date(notification_date),as_date(patient_dob), units = "days"))/365.25
    )  
  )%>%
  mutate(agecategory = case_when(
    age_dob < 0 ~ "Unknown",
    age_dob %in% 0:4 ~ "0-4",
    age_dob %in% 5:9 ~ "5-9",
    age_dob %in% 10:14 ~ "10-14",
    age_dob %in% 15:19 ~ "15-19",
    age_dob %in% 20:24 ~ "20-24",
    age_dob %in% 25:29 ~ "25-29",
    age_dob %in% 30:34 ~ "30-34",
    age_dob %in% 35:39 ~ "35-39",
    age_dob %in% 40:44 ~ "40-44",
    age_dob %in% 45:49 ~ "45-49",
    age_dob %in% 50:54 ~ "50-54",
    age_dob %in% 55:59 ~ "55-59",
    age_dob %in% 60:64 ~ "60-64",
    age_dob %in% 65:200 ~ "65+",
    is.na(age_dob) ~ "Unknown"
  ))%>%
  mutate(agecategory = factor(agecategory, levels = c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65+", "Unknown")))
  
data14.2
# Clean age variables
data15 <- data14.2 %>%
  mutate(patient_age = as.integer(patient_age)) %>%
  mutate(agecategory_unit = case_when(
    Age_years < 0 ~ "Unknown",
    Age_years %in% 0:4 ~ "0-4",
    Age_years %in% 5:9 ~ "5-9",
    Age_years %in% 10:14 ~ "10-14",
    Age_years %in% 15:19 ~ "15-19",
    Age_years %in% 20:24 ~ "20-24",
    Age_years %in% 25:29 ~ "25-29",
    Age_years %in% 30:34 ~ "30-34",
    Age_years %in% 35:39 ~ "35-39",
    Age_years %in% 40:44 ~ "40-44",
    Age_years %in% 45:49 ~ "45-49",
    Age_years %in% 50:54 ~ "50-54",
    Age_years %in% 55:59 ~ "55-59",
    Age_years %in% 60:64 ~ "60-64",
    Age_years %in% 65:200 ~ "65+",
    is.na(Age_years) ~ "Unknown"
  ))%>%
  mutate(agecategory_unit = factor(agecategory_unit, levels = c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
    "60-64", "65+", "Unknown")))

# age category sensitivity analysis
xtabs(~ data15$agecategory_unit)
xtabs(~ data15$agecategory)
xtabs(~ data15$patient_age_category)


data15$agecategory%>%unique
summary(data15$Age_years)
hist(data15$Age_years)

#xtabs(~ age_group, data = data15)
xtabs(~ agecategory, data = data15)
xtabs(~Age_years+agecategory, data = data15%>%filter(agecategory == "0-4")) 
xtabs(~patient_age+agecategory + patient_age_unit, data = data15%>%filter(agecategory == "0-4")) 

# Clean gender variable
data16 <- data15 %>%
  mutate(
    gender = ifelse(!patient_gender %in% c("Male", "Female"), "Unknown", patient_gender))


# Create a data frame with the condition column
dataCat1 <- data.frame(condition = c("Acute Flaccid Paralysis", "Acute rheumatic fever", "Botulism", "Cholera",
                                 "Crimean-Congo viral haemorrhagic fever (human)", "Ebola Virus (VHF)",
                                 "Lassa Fever Virus(VHF)", "Lujo Virus(VHF)", "Marburg Virus (VHF)",
                                 "Diphtheria", "Enteric fever (typhoid or paratyphoid fever)",
                                 "Food borne illness outbreak", "Listeriosis", "Malaria", "Measles",
                                 "Meningococcal Disease", "Pertussis", "Plague", "Rabies", "Smallpox",
                                 "VHF Other", "Rift Valley Fever", "Yellow Fever",
                                 "Waterborne illness outbreak - UNDEFINED", "Congenital rubella syndrome",
                                 "Rubella", "Haemolytic uraemic syndrome (HUS)",
                                 "Agricultural or stock remedy poisoning", "Bilharzia (schistosomiasis)",
                                 "Brucellosis", "Congenital syphilis", "Haemophilus influenzae type B",
                                 "Hepatitis A", "Hepatitis B", "Hepatitis C", "Hepatitis E", "Legionellosis",
                                 "Leprosy", "Lead poisoning", "Mercury poisoning",
                                 "Maternal death (pregnancy, childbirth and puerperium)",
                                 "Soil transmitted helminths", "Tetanus", "Tuberculosis:pulmonary",
                                 "Tuberculosis:extra-pulmonary", "Tuberculosis: extensively drug -resistant (XDR -TB)",
                                 "Tuberculosis: multidrug- resistant (MDR -TB)",
                                 "Endemic arboviral diseases Chikungunya virus",
                                 "Endemic arboviral diseases West Nile Virus",
                                 "Non-Endemic Arboviral Diseases: Zika Virus",
                                 "Non-Endemic arboviral diseases : Dengue Fever Virus",
                                 "Non-typhoidal Salmonellosis", "Shiga toxin-producing Escherichia coli",
                                 "Shigellosis", "Endemic arboviral diseases Sindbis Virus"))

# Create the nmccategories column based on the conditions
data17 <- data16 %>%
  mutate(nmccategories = case_when(
    condition %in% c("Acute Flaccid Paralysis", "Acute rheumatic fever", "Botulism", "Cholera",
                     "Crimean-Congo viral haemorrhagic fever (human)", "Ebola Virus (VHF)",
                     "Lassa Fever Virus(VHF)", "Lujo Virus(VHF)", "Marburg Virus (VHF)",
                     "Diphtheria", "Enteric fever (typhoid or paratyphoid fever)",
                     "Food borne illness outbreak", "Listeriosis", "Malaria", "Measles",
                     "Meningococcal Disease", "Pertussis", "Plague", "Rabies", "Smallpox",
                     "VHF Other", "Rift Valley Fever", "Yellow Fever",
                     "Waterborne illness outbreak - UNDEFINED", "Congenital rubella syndrome",
                     "Rubella", "Haemolytic uraemic syndrome (HUS)") ~ 1,
    condition %in% c("Agricultural or stock remedy poisoning", "Bilharzia (schistosomiasis)",
                     "Brucellosis", "Congenital syphilis", "Haemophilus influenzae type B",
                     "Hepatitis A", "Hepatitis B", "Hepatitis C", "Hepatitis E", "Legionellosis",
                     "Leprosy", "Lead poisoning", "Mercury poisoning",
                     "Maternal death (pregnancy, childbirth and puerperium)",
                     "Soil transmitted helminths", "Tetanus", "Tuberculosis:pulmonary",
                     "Tuberculosis:extra-pulmonary", "Tuberculosis: extensively drug -resistant (XDR -TB)",
                     "Tuberculosis: multidrug- resistant (MDR -TB)") ~ 2,
condition %in% c("Endemic arboviral diseases Chikungunya virus",
                 "Endemic arboviral diseases West Nile Virus",
                 "Non-Endemic Arboviral Diseases: Zika Virus",
                 "Non-Endemic arboviral diseases : Dengue Fever Virus",
                 "Non-typhoidal Salmonellosis", "Shiga toxin-producing Escherichia coli",
                 "Shigellosis", "Endemic arboviral diseases Sindbis Virus") ~ 3,
.default = 4))%>%filter(nmccategories %in% c(1:3))

# Display frequency tables
table(data17$nmccategories)
table(data17$condition, data17$nmccategories)

# Exlude cat4?

library(tidyverse)

# Checking the plausibility of dates
# codebook symptom_date diagnosis_date notification_date
# Symptom date, diagnosis date, date of notification, date specimen taken, date patient death are all in numeric daily dates
# type = numeric daily date (int)
data17 %>%
  filter(symptom_date > diagnosis_date) %>%
  count()
# n = 0

data18 <- data17 %>%
  arrange(diagnosis_date, notification_date, symptom_date)
# ed

data18 %>%
  filter(diagnosis_date > notification_date) %>%
  count()
# n = 4

data19 <- data18 %>%
  arrange(diagnosis_date, notification_date, symptom_date, condition, case_id, case_source)
# ed

# Generate additional date-related variables
data20 <- data19 %>%
  mutate(time_to_notification = as.numeric(difftime(as_date(notification_date), as_date(diagnosis_date), units = "days"))) %>%
  arrange(time_to_notification, symptom_date, diagnosis_date, notification_date)

# Clerically edit cases where time_to_notification is a negative value
# n = 0

#save(data, file = "path/to/file.csv") # Replace "path/to/file.csv" with the desired file path and name

data21 <- data20 %>%
  mutate(
    Year_symptoms = as_year(symptom_date),
         Month_symptoms = as_month(symptom_date),
         Year_diagnosis = as_year(diagnosis_date),
         Month_diagnosis = as_month(diagnosis_date),
         Year_notification = as_year(notification_date),
         Month_notification = as_month(notification_date)
    )

#data21 <- data20 %>%
#  mutate(
#    Year_symptoms = as_year(coalesce(symptom_date,as_date(substr(excel_load$case_id, 1, 6), format = "%y%m%d"))),
#    Month_symptoms = as_month(coalesce(symptom_date, as_date(substr(excel_load$case_id, 1, 6), format = "%y%m%d"))),
#    Year_diagnosis = as_year(coalesce(diagnosis_date, as_date(substr(excel_load$case_id, 1, 6), format = "%y%m%d"))),
#    Month_diagnosis = as_month(coalesce(diagnosis_date, as_date(substr(excel_load$case_id, 1, 6), format = "%y%m%d"))),
#    Year_notification = as_year(coalesce(notification_date, as_date(substr(excel_load$case_id, 1, 6), format = "%y%m%d"))),
#    Month_notification = as_month(coalesce(notification_date, as_date(substr(excel_load$case_id, 1, 6), format = "%y%m%d")))
#  )


# Epiweek is better calculated in Excel, R tends to overestimate the number of weeks


# Duplicate and manual linking
data21 %>%
  count()
# 10,625

data22<- data21 %>%
  mutate(facility = tolower(facility))

#save(data, file = "March2023_beforededuplication.csv", replace = TRUE)  # Replace "March2023_beforededuplication.csv" with the desired file path and name

# Edit case_type to record manually linked cases
data23 <- data22 %>%
  group_by(case_id) %>%
  mutate(duplicate = ifelse(n() > 1, "duplicate", "unique"),
         dup_number = row_number()) %>%
  ungroup()

data_dup <- data23 %>%
  filter(dup_number == 1)
# n = 105 - some of these are different patients with the same NMC CaseID number - cannot just drop duplicates - extract for reporting purposes
nrow(data_dup)

# find a way to identify duplciates by condition and name/surname, 
# and 
# duplicates of people with different conditions. 

# Calling direct duplicates
names(data_dup)
data_dup2 <- data_dup %>%
  arrange(condition, case_type, epidemiological_classification, facility, patient_name, patient_surname, gender, patient_dob) %>%
  group_by(condition, case_type, epidemiological_classification, facility, patient_name, patient_surname, gender, patient_dob) %>%
  mutate(duplicate = ifelse(n() > 1, "duplicate", "unique"),
         dup_number = row_number()) %>%
  ungroup()

data_dup3 <- data_dup2 %>%
  filter(dup_number == 1)


data_dup3 <- data_dup3 %>%
  arrange(condition, episode_number, case_id, notification_date, patient_name, patient_surname, patient_dob, folder_no, facility, province, gender)
# ed

# Double-check that there are no cases that need manual linking before you drop
# If you want to drop all duplicate observations but keep the first occurrence, use: data <- data %>% drop_duplicates()
data_dup3 %>%
  filter(dup_number == 1)

count(data_dup3)

library(dplyr)

# Check cases that missed the automated linkage

###################
# Deduplciation process. 
##################

# first dedupicate by condition , facility, name, sunrame, gender and dob
data_dup4 <- data_dup3 %>% 
  group_by(condition, facility, patient_name, patient_surname, gender, patient_dob) %>% 
  mutate(duplicate = ifelse(n() > 1, "duplicate", "unique"),
         dup_number = row_number()) %>%
  ungroup()

xtabs(~ dup_number, data = data_dup4)
xtabs(~ duplicate, data = data_dup4)

# keep only the first occurence
data_dup5 <- data_dup4 %>%
  filter(dup_number == 1)%>%
  arrange(dup_number, condition, episode_number, case_id, notification_date, case_type, 
          patient_name, patient_surname, patient_dob, facility, folder_no, province, gender) 

data_dup5

save(data_dup5, file = "data_dup6.rda")


#data_dup6<- data_dup5%>%filter(!condition == "Respiratory disease caused by a novel respiratory pathogen")

#data_dup6$condition%>%unique
#drop Respiratory pathogen 


########################
########################

data_dup5 %>% 
  arrange(case_type) %>%
  filter(duplicate == "duplicate" & case_type == "Merged") %>%nrow()
# there are no duplciates in merged

#drop cases that are duplciates and lab case types
data_dup6 <- data_dup5%>%
  filter(!(duplicate == "duplicate" & case_type == "Lab"))


data_dup5 %>% 
  count()

data_dup6 %>% 
  count()


# Now deduplcaite using mor relaxed variables. 
data_dup7<- data_dup6 %>% 
  group_by(condition, facility, patient_name, patient_surname, gender) %>%
  mutate(duplicate = ifelse(n() > 1, "duplicate", "unique"),
       dup_number = row_number()) %>%
  ungroup()

xtabs(~dup_number, data_dup7)

#no duplicates

data_dup8 <- data_dup7 %>% 
  group_by(condition, facility, patient_name, patient_surname, folder_no, gender) %>% 
  mutate(duplicate = ifelse(n() > 1, "duplicate", "unique"),
         dup_number = row_number()) %>%
  ungroup()

xtabs(~dup_number, data_dup8)


# remove those without folder numebrs. 
data_dup9 <- data_dup8 %>% 
  arrange(folder_no) %>%
  arrange( condition, folder_no, episode_number, case_id, notification_date, case_type, 
          patient_name, patient_surname, patient_dob, facility, province, gender) %>%
  #filter(duptag3 > 0) %>%
  filter(folder_no != ""| !is.na(folder_no))


data_dup9 %>% 
  count()


library(dplyr)

# Facilities are different for referred patients so we dedupclaite without facility. 
data_dup10 <- data_dup9 %>% 
  group_by(condition, patient_dob, patient_name, patient_surname, gender, patient_age) %>% 
  mutate(duplicate = ifelse(n() > 1, "duplicate", "unique"),
         dup_number = row_number()) %>%
  ungroup()

xtabs(~dup_number, data_dup9)

data_dup9%>%
  count()

data_dup10 %>% 
  count()

#data_dup10%>%filter(dup_number == 1)

# Now include facility, dob etc. . 

data_dup11 <- data_dup10 %>% 
  group_by(condition, facility, patient_dob, patient_name, patient_surname) %>% 
  mutate(duplicate = ifelse(n() > 1, "duplicate", "unique"),
         dup_number = row_number()) %>%
  ungroup()

xtabs(~dup_number, data_dup11)

data_dup12 <- data_dup11%>%filter(dup_number ==1)


# Now exclude the condition. this will exclude repeat TB cases though. # i guess if it happens within the same month there may be some kind of error

data_dup13 <- data_dup12 %>% 
  group_by(patient_name, patient_surname, patient_dob, facility) %>% 
  mutate(duplicate = ifelse(n() > 1, "duplicate", "unique"),
         dup_number = row_number()) %>%
  ungroup()

xtabs(~dup_number, data_dup13)

data_dup14<- data_dup13%>%filter(dup_number == 1)

data_dup14 %>% 
  count()


data_dup15 <- data_dup14 %>% 
  group_by(condition, case_source, folder_no, facility, patient_dob) %>% 
  mutate(duplicate = ifelse(n() > 1, "duplicate", "unique"),
         dup_number = row_number()) %>%
  ungroup()

xtabs(~dup_number, data_dup15)

data_dup16<-data_dup15%>%filter(dup_number == 1)

data_dup16
#######################

#######################


  
  
  
data_dup17 <- data_dup16%>% 
  group_by(patient_surname, patient_dob, condition, folder_no) %>% 
  mutate(duplicate = ifelse(n() > 1, "duplicate", "unique"),
         dup_number = row_number()) %>%
  ungroup()

xtabs(~dup_number, data_dup17)

data_dup18<-data_dup17%>%filter(dup_number == 1)


data_dup18 %>% 
  count()

#write.csv(df, "March2023_afterdeduplication.csv", row.names = FALSE)


library(dplyr)
colSums(is.na(data_dup18))

data_dup18
data_dup18 %>%filter(is.na(symptom_date))


# Cleaning up case source

data_dup18%>% filter(epidemiological_classification == "Confirmed case" & !is.na(symptom_date) & !is.na(notifier) & !is.na(episode_number))

data_dup18%>% filter(  !grepl("*Other", symptoms, ignore.case = T))

#!grepl("*Other*", symptoms, ignore.case = T)

data_dup18$symptoms%>%unique()
data_dup18$case_type%>%unique
data_dup18$epidemiological_classification%>%unique

data_dup19 <- data_dup18%>%
mutate(case_type = case_when(
    epidemiological_classification == "Confirmed case" & !is.na(symptom_date) & !is.na(notifier) & !is.na(episode_number) ~ "Merged cases",
    epidemiological_classification == "Confirmed case" & !is.na(symptom_date) & !is.na(treatment) ~ "Merged cases",
    epidemiological_classification == "Suspected Case" & is.na(episode_number) ~ "Clinical notifications",
    epidemiological_classification == "Probable case" & is.na(episode_number) ~ "Clinical notifications",
    case_type == "Lab" & !is.na(symptom_date)  & !is.na(diagnosis_date)& !is.na(treatment) & !is.na(symptoms) ~ "Merged cases",
    case_type == "Lab" & !is.na(symptom_date)  & !is.na(diagnosis_date) & is.na(treatment) & is.na(symptoms) ~ "Merged cases",
    case_type == "Lab" & !is.na(treatment) ~ "Merged cases",
    case_type == "Lab" & !is.na(symptom_date) & !is.na(diagnosis_date) ~ "Merged cases",
    case_type == "Lab" & !is.na(diagnosis_date) & !grepl("*Other*", symptoms, ignore.case = T) ~ "Merged cases",
    case_type == "Lab" & !is.na(symptom_date) & grepl("*Other*", symptoms, ignore.case = T) ~ "Merged cases",
    case_type == "Lab" & epidemiological_classification == "Laboratory notification" ~ "Laboratory notifications",
    case_type == "Clinical" & episode_number == "" ~ "Clinical notifications",
    TRUE ~ case_type
  ))%>%
  mutate(case_type = case_when(grepl("^lab",case_type,  ignore.case = T ) ~ "Laboratory notifications",
                               grepl("^merge",case_type,  ignore.case = T) ~ "Merged Cases", 
                               grepl("^clin",case_type,  ignore.case = T) ~ "Clinical notifications"))



data_dup19$case_type %>%unique
xtabs(~ case_type, data = data_dup19)

xtabs(~ case_type+ epidemiological_classification,  data_dup19, addNA = T)
xtabs(~ case_type+ epidemiological_classification,  data_dup19, addNA = T)

# Drop CS lab notification according to case_source
data_dup20 <- data_dup19 %>%
  filter(!(condition == "Congenital syphilis" & case_type == "Lab"))

# Drop cases of Congenital syphilis above age 2
data_dup21 <- data_dup20 %>%
  filter(!(condition == "Congenital syphilis" & patient_age > 2))

# Check the counts
count(data_dup21)

library(dplyr)

# Malaria notifications
malaria <- data_dup21%>%
  filter(condition == "Malaria")

xtabs(~ case_type+diagnosis_method, data = malaria)
nrow(malaria)


data_dup22 <- data_dup21 %>%
  filter(!(condition == "Malaria" & diagnosis_method == "Clinical signs and symptoms ONLY|Other" & is.na(episode_number)))

nrow(data_dup22)


data_dup23 <- data_dup22 %>%
  mutate(diagnosis_method = case_when(
    condition == "Malaria" & diagnosis_method == "Clinical signs and symptoms ONLY" & !is.na(episode_number) ~ "Laboratory confirmed",
    condition == "Malaria" & diagnosis_method == "Other" & !is.na(episode_number) ~ "Laboratory confirmed",
    condition == "Malaria" & diagnosis_method == "Laboratory confirmed" ~ "Laboratory confirmed",
    condition == "Malaria" & diagnosis_method == "UNKNOWN" ~ "Laboratory confirmed",
    TRUE ~ diagnosis_method
  ))

data_dup24 <- data_dup23 %>%
  filter(!(condition == "Malaria" & grepl("X-ray|Rapid test", diagnosis_method, ignore.case = T )))

xtabs(~ condition+diagnosis_method, data =data_dup24 )

data_dup25 <- data_dup24 %>%
  mutate(diagnosis_method = case_when(
    condition == "Malaria" & !is.na(episode_number) ~ "Laboratory confirmed",
    TRUE ~ diagnosis_method
  ))

data_dup25 %>%
  filter(condition == "Malaria") %>%
  select(epidemiological_classification, diagnosis_method) %>%
  group_by(epidemiological_classification, diagnosis_method) %>%
  tally()


######################
# Creating back acapture vars. 
######################
names(data_dup25)
xtabs(~data_dup25$Year_diagnosis)
xtabs(~data_dup25$Month_diagnosis)
xtabs(~data_dup25$diagnosis_date)
xtabs(~is.na(data_dup25$diagnosis_date))

data_dup26 <- data_dup25 %>%
  mutate(Back_capture = case_when(
    case_type == "Lab" & is.na(diagnosis_date) ~ "Current",
    as_date(diagnosis_date) > as_date(reporting_date) ~ "Current",
    as_date(diagnosis_date )< (as_date(reporting_date) - days(14)) ~ "Back capture",
    as_date(diagnosis_date) %in%  (as_date(reporting_date) - days(14)) : (as_date(reporting_date)) ~ "Delayed",
    TRUE ~ "unkown"
  ))


# province to create Prov_


data_dup27<- data_dup26%>%
  mutate(prov_ =sapply(strsplit(province, " "), function(x) x[1]),
                                 age = patient_age)%>%
  mutate(province  = ifelse(is.na(province),sub("^[^ ]+ ", "", patient_province) , province))

xtabs(~ is.na(data_dup27$prov_))
xtabs(~ is.na(data_dup27$prov_))
data_dup27%>%filter( is.na(prov_)) %>%view()

data_dup27%>%filter(condition == "Food borne illness outbreak")

# need directionon this one. 
data_dup27 %>%
  mutate(NotFBO = if_else(condition == "Food borne illness outbreak", "", as.character(NA))) %>%
  arrange(facility) %>%
  group_by(facility) %>%
  mutate(NotFBO = if_else(row_number() == 1, "1", NotFBO)) %>%
  ungroup() %>%
  filter(is.na(NotFBO))

#data_dup27 %>%
#  filter(!(condition == "Food borne illness outbreak" & !(NotFBO == "1")))

data_dup27 %>%
  filter(condition == "Cholera")

data_dup27 %>%
  count()
data_dup27%>%names()

data_dup28<- data_dup27 %>%
  select(nmccategories, condition, case_id, case_type, 
         epidemiological_classification, facility, facility_sector, facility_type, 
         facility_classification, district, sub_district, province, prov_, notifier, 
         case_source, symptom_date, Year_symptoms, Month_symptoms, diagnosis_date, Year_diagnosis, Month_diagnosis,
         diagnosis_method, notification_date, Year_notification, Month_notification, time_to_notification, Back_capture, 
         folder_no, patient_name, patient_surname, patient_dob, age, agecategory, gender, pregnancystatus, 
         id_type, patient_id_no, 
         patient_passport_no, patient_contact_no, patient_country, patient_province, patient_suburb, patient_city, symptoms, 
         treatment, patient_vital_status, patient_death_date, patient_admission_status, patient_has_travelled, travelhistory, 
         vaccination_status, vaccination_last_date, specimen_collected, specimen_barcode, episode_number,
         admissiondate, ethnicgroup, cchypertension) %>%
  arrange(Back_capture, diagnosis_date) #%>%
  #save(file = "output_file.csv", overwrite = TRUE)


library(tidyverse)

# Identifying completed hospital form
data_dup27%>%names()

colSums(is.na(data_dup27%>%filter(patient_admission_status == "Inpatient") %>%select(admissiondate, ethnicgroup, cchypertension, admissionward, otheradmissiontreatment,
                                                                                     ccdiabetes, patient_has_travelled,  )))

xtabs(~ patient_admission_status, data = data_dup27, addNA = T)


data_dup29 <- data_dup27 %>%mutate(
  HFcomplete = case_when( !is.na(admissiondate) ~ "Yes", 
                          nmccategories == 1 & patient_admission_status == "Inpatient" ~ "Yes",
                          !is.na(ethnicgroup ) ~ "Yes", 
                          !is.na(cchypertension )~ "Yes", 
  .default = "No" ))%>%
  mutate(HFcomplete = if_else(patient_admission_status == "Discharged" & HFcomplete == "" & nmccategories == 1, "No", HFcomplete)) %>%
  mutate(HFcomplete = if_else(patient_admission_status == "Inpatient" & HFcomplete == "" & nmccategories == 1, "No", HFcomplete)) %>%
  mutate(HFcomplete = if_else(patient_admission_status == "Transferred" & HFcomplete == "" & nmccategories == 1, "No", HFcomplete)) %>%
  mutate(HFcomplete = if_else(patient_admission_status == "Unknown" & HFcomplete == "" & nmccategories == 1, "NA", HFcomplete)) %>%
  mutate(HFcomplete = if_else(patient_admission_status == "Outpatient" & HFcomplete == "" & nmccategories == 1, "NA", HFcomplete))


  
# Count patient_admission_status

xtabs(~ patient_admission_status, data = data_dup29)

# Count patient_admission_status by HFcomplete

xtabs(~ patient_admission_status +HFcomplete, data = data_dup29, addNA = T)

# Filter conditions
data_dup29 %>%
  filter(patient_admission_status != "Outpatient" & patient_admission_status != "Other") %>%
  filter(HFcomplete != "Yes")

# Data quality report dofile
# Calculate completeness
data_dup29$case_source
xtabs(~data_dup29$gender, addNA = T)

data_dup30 <- data_dup29 %>%
  mutate(source_2 = ifelse(grepl("^clinical", case_type, ignore.case = T) | grepl("^merge", case_type, ignore.case = T)  ,  "Clinical & Merged","Lab only"))%>%
  
  mutate(patient_admission_status = ifelse(is.na(patient_admission_status), "Unknown", patient_admission_status),
         patient_vital_status = ifelse(is.na(patient_vital_status), "Unknown", patient_vital_status)
         )
  

xtabs(~ case_type + source_2, data = data_dup30)
xtabs(~ patient_admission_status , data = data_dup30, addNA = T)
xtabs(~ patient_vital_status , data = data_dup30, addNA = T)

data_dup31 <- data_dup30 %>% # adjust this so that the android etc is under app. 
  mutate(capture_type = case_source) %>%
  mutate(capture_type = recode(capture_type, "NMC Reporting Android" = "App", "NMC Reporting iOS" = "App", "NMC Reporting Web" = "App", "SDW" = "App")) %>%
  mutate(capture_type = recode(capture_type, "SDW" = "Lab"))%>%
  mutate( censor = case_when(patient_vital_status == "Deceased" ~ "Deceased", 
                            .default =  "Not Deceased"))%>%
  mutate(censor = factor(censor, levels = c("Not Deceased", "Deceased")))%>%
  mutate(capture_type2 = case_when(capture_type == "Android" | capture_type == "iOS" |capture_type == "Web"  ~ "App",
                                   capture_type == "Microstrategy/SDW" & case_type == "Merged" ~ "App",
                                    .default = capture_type))


xtabs(~ capture_type2+ case_type, data = data_dup31)
xtabs(~ capture_type+ case_type, data = data_dup31)
xtabs(~ capture_type+ source_2, data = data_dup31)
xtabs(~ censor, data = data_dup31, addNA = T)

xtabs(~ condition+capture_type2 , data = data_dup31)
xtabs(~ condition+case_type , data = data_dup31)

##################################################################
# Variable cleaning
##################################################################

data_dup32 <- data_dup31 %>%
  mutate(province = sub("^[^ ]+ ", "", province),
         facility_sector = str_to_title(facility_sector))


xtabs(~ data_dup32$agecategory)
xtabs(~ data_dup32$agecategory_unit)

return(data_dup32)
}

# Step 3: Store the DataFrame in the object with the created name
# Replace 'your_data_frame' with your actual DataFrame name

#assign(get_object_name_NMC(), data_dup32)

# Now, you can access the DataFrame using the object name with the prefix and last month's date
# For example:
# NMC_2023_06 <- your_data_frame
# You can use NMC_2023_06 as a regular object name to work with the DataFrame.





