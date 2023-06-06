# ExECT z output diagnosis processing

library(readr)
 # not in te correct order
library(RSQLite)
library(sqldf)
library(proto)
library(gsubfn)
library(dplyr)
library(tibble)
library(zoo)
library(lubridate)

options(scipen = 999) #numbers should not be displayed in scientific notation
options(digits = 6)


setwd("C:/Users/Beata/Documents/ExECT_Z_Output")# set working dir

# Diagnosis 

Diagnosis <- read.csv(file = "Diagnosis.csv", header = FALSE)


# Adding the header

colnames(Diagnosis) <- c("Letter","Start","End","CUI","PREF","Negation","DiagCategory", "Certainty","Rule","DoBDD","DoBMD",	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode", "Last")

Diagnosis <-Diagnosis %>% 
select(- "Last") # removing that last column but when running the output remove the last "," from groovy
  
View(Diagnosis) 

#adding a column Diagnosis, where Epilepsy = diag with certainty 4/5 and multiple seizures
# For ExECT output = EpiDiag

EpiDiag <- Diagnosis %>% 
  mutate(Diag = case_when(Certainty  == "4" &  DiagCategory != "SingleSeizure" ~ "Epilepsy",
                       Certainty == "5" &  DiagCategory != "SingleSeizure"  ~ "Epilepsy",
                       DiagCategory == "SingleSeizure" ~ "NO"))
View(EpiDiag) # records for multiple seizures and epilepsy of all certainty but column Diagnosis
                # identifies those who have Epilepsy diag as no single seizures or certainty <4

#This can be filtered to letter, start, diag - you need START as this identifies individual annotations

EpiDiagLet <- EpiDiag %>% 
  select(Letter, Start, Diag, NHS_No) %>% 
  distinct() 

View(EpiDiagLet)

# or to only letter and diagnosis to do per letter score
EpiDiagLet2 <- EpiDiag %>% 
  select(Letter, Diag) %>%   # here it gives 683 from 771 letters
  distinct() 

EpiDiagLet2 %>% 
  arrange(Letter, Diag)

# identifying letters that have both Epilepsy diag and NO, so creating a column Epilepsy that would assign epilepsy if two records exist - one of each
EpiDiagLetter <- EpiDiagLet2 %>% 
mutate(Epilepsy =case_when(Letter == lag(Letter) & lag(Diag) != "NO" & (Diag == "NO" | is.na(Diag)) ~ lag(Diag),
                           Letter == lead(Letter) & lead(Diag) != "NO" & (Diag == "NO" | is.na(Diag)) ~ lead(Diag),
                           Diag == "Epilepsy" ~ "Epilepsy" ))

#removing Negative records if positive exist
EpiDiagLetter <- EpiDiagLetter %>% 
  select(Letter, Epilepsy) %>% 
  distinct() # this gives all letters with Epilepsy, or NA , but no letters that ther are both of them


#Diagnosis of epilepsy = all epilepsy diagnosis including non-specific epilepsy but not seizures 

EpiDiagEpilepsy <- EpiDiag %>% 
  filter(Diag == "Epilepsy", DiagCategory == "Epilepsy") %>%   
select(Letter, Start, CUI, NHS_No) %>%  #just reducing
select(NHS_No) 

EpiDiagEpilepsy$NHS_No<-gsub(" ", "", as.character(EpiDiagEpilepsy$NHS_No))
EpiDiagEpilepsy$NHS_No<-gsub("-", "", as.character(EpiDiagEpilepsy$NHS_No))

#gives you the number of people (with NHS_No) with epilepsy 
PeopleEpilepsy <- EpiDiagEpilepsy %>% 
  select(NHS_No) %>% 
  filter(NHS_No != "null") %>% 
  distinct()   

View(PeopleEpilepsy)
 

# Epilepsy type based on epilepsy or syndrome i.e. removing non-specific epilepsy types



# Epilepsy =C0014544  drug-refractory-epilepsy = C1096063" , symptomatic_epilepsy = "C1406659", post-traumatic_epilepsy= "C0014557", photosensitive-epilepsy !="C0393720"
EpiType <- EpiDiag %>% 
  filter(DiagCategory == "Epilepsy") %>% 
  filter(CUI != "C0014544", CUI != "C1096063" , CUI != "C1406659", CUI != "C0014557", CUI !="C0393720")

EpiTypeP <- EpiType %>% 
select(Letter, CUI, NHS_No) %>%  # this gives the number of letters with specific epilepsy type/ syndrome 
select(NHS_No)

EpiType$NHS_No<-gsub(" ", "", as.character(EpiType$NHS_No))
EpiType$NHS_No<-gsub("-", "", as.character(EpiType$NHS_No))

#gives you the number of people (with NHS_No) with specific epilepsy type / syndrome
PeopleEpiType <- EpiType %>% 
  select(NHS_No) %>% 
  filter(NHS_No != "null") %>% 
  distinct()   


View(PeopleEpiType)



# Bringing in Epilepsy_Seizure_list so that CUI can be grouped


Epilist <- Epilepsy_Seizure_list %>% 
  select(CUI, EpilepsyType) %>% 
  distinct()

View(Epilist)

# selecting again epilepsy type group so CUIs can be grouped
EpiTypeGr <- EpiType %>% 
  select(Letter, CUI, NHS_No)

EpiTypeGr$CUI<-gsub(" ", "", as.character(EpiTypeGr$CUI))

EpiTypeGroups <- left_join(EpiTypeGr, Epilist)

View(EpiTypeGroups)


# Epilepsy type by letter
EpiTypeGroups$NHS_No<-gsub(" ", "", as.character(EpiTypeGroups$NHS_No))

EpiTypeGroups$NHS_No<-gsub("-","", as.character(EpiTypeGroups$NHS_No))


EpiTypeGroupsNHS_No <- EpiTypeGroups %>% 
  select(NHS_No, EpilepsyType) %>% 
distinct() 



View(EpiTypeGroupsNHS_No)

EpiTypeGroupsNHS_NoF <- EpiTypeGroupsNHS_No %>% 
  filter(EpilepsyType == "Focal") %>% 
count()

View(EpiTypeGroupsNHS_NoF) # 61

head(EpiTypeGroupsNHS_NoF)

EpiTypeGroupsNHS_NoG <- EpiTypeGroupsNHS_No %>% 
  filter(EpilepsyType == "Generalised") %>% 
count()   #36
View(EpiTypeGroupsNHS_NoG)

head(EpiTypeGroupsNHS_NoG)













 



