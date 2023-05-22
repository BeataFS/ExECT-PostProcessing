# ExECT precessing using the z drive output. As IDEx was run as part of the ExECT pipeline we need to include 
# clinic date letter date NHS, DoB and Hosp number are part of the outputs.
# after creating all the output files (here used groovy and gate developer)

library(readr)
library(dplyr)
# library(zoo) #for dates = but experimented here with origin
library(lubridate)
library(readxl)

options(scipen = 999) #numbers should not be displayed in scientific notation
options(digits = 6) #Number of digits to be displayed

setwd("C:/Users/Beata/Documents/ExECT_Z_Output")# set working dir

# 1 total number of people in the cohort----

# We are using NHS number to identify individuals, we are counting the number of distinct NHS numbers in all the documents. 
# It is possible that there are people without a single letter that has their NHS number, those would be missed, but if we select the documents without
# the NHS number and check for DOB, Hospital Number and PostCode we may be able to identify those and assign them with an NHS No

# first we need to upload all output files and select NHS number (some people may have only a single annotation type so we need all outputs)

# 1a Seizure Frequency (SF)----



SF <- read.csv(file = "SeizureFrequency.csv", sep = ",", na = c("","NA","null", "NULL"), header = FALSE) # may need to add sep ":" as sometimes if file was opened in excel it gets ':' as separators

# Adding headers, we may also need to concatenate clinic date, letter date, and DoB - later!

colnames(SF) <- c("Letter","Start","End","CUI","String","FreqChange",	"NofS",	"LNofS"	,"UNofS","TP",	"NofTP","LNofTP",	"UNofTP",	"Sin_Dur","YD",	"MD",	"DD",	"PinT",	
                  "Age","AgeLower",	"AgeUpper",	"AgeUnit", "Certainty",	"Negation",	"Rule",	"DoBDD",	"DoBMD",	"DoBYD"	,"ClinicDD",	"ClinicMD","ClinicYD",	"LetDD",	
                  "LetMD",	"LetYD","Hosp_No","NHS_No","PostCode", "Gender", "Address", "FirstName", "MiddleName", "Surname")

SF %>% 
  mutate_at(vars( NofS, LNofS,UNofS,NofTP,LNofTP,UNofTP, YD, MD, DD, Age, AgeLower, AgeUpper,DoBDD, DoBMD, DoBYD, ClinicDD, ClinicMD, ClinicYD), as.numeric)


# Creating a complete year out of two digit years under DoBYD2 i.e. 73 into 1973

SF_1 <- SF %>%
  mutate(DoBYD2 = case_when(DoBYD >1000 ~ as.numeric(DoBYD),
                            DoBYD <100 ~as.numeric(DoBYD+1900),
                            DoBYD < 20 ~ as.numeric(DoBYD+2000)), .after = "DoBYD")


#View(SF_1)

# This works with the assumption that we do not have letters for people born before 1920,
# however when we get older letter this may not be the case - we will have to chack against the
# clinic date, so it may be that once we get the full DoB as 00YY we will have to compare
# the year to the year of the clinic and for those before 2000 get the DoB year as 19YY

# Creating a single date field

SF_2 <- SF_1 %>% 
  mutate(DoB = make_date(day = DoBDD, month = DoBMD, year = DoBYD2),.after = "DoBYD2" ) 



# clinic date
# same problem, year date may be written as yy so need to convert to yyyy ubder 
# ClinicYD2 and then make date


SF_3 <- SF_2 %>%
  mutate(ClinicYD2 = case_when(ClinicYD >1000 ~ as.numeric(ClinicYD),
                               ClinicYD <25 ~ as.numeric(ClinicYD+2000),
                               ClinicYD >25 ~ as.numeric(ClinicYD+1900)), .after = "ClinicYD")


SF_4 <- SF_3 %>% 
  mutate(Clinic_Date = make_date(day = ClinicDD, month = ClinicMD, year = ClinicYD2 ),.after = "ClinicYD2") 

# Letter date


SF_5 <- SF_4 %>%
  mutate(LetYD2 = case_when(LetYD >1000 ~ as.numeric(LetYD),
                            LetYD <25 ~ as.numeric(LetYD+2000),
                            LetYD >25 ~ as.numeric(LetYD+1900)), .after = "LetYD")


SF_6 <- SF_5 %>% 
  mutate(Let_Date = make_date(day = LetDD, month = LetMD, year = LetYD2 ),.after = "LetYD2") 

# creating record date = clinic date but if this is missing letter date

SF_7 <-SF_6 %>% 
  mutate(DATEREC = case_when(!is.na(Clinic_Date) ~ Clinic_Date,
                             is.na(Clinic_Date) & !is.na(Let_Date) ~ Let_Date),.after = "Let_Date")


str(SF_7)


# removing spaces and "-" from NHS numbers ( for File 1 no spaces) and Hospital numbers

SF_7$NHS_No <- gsub(" ", "", as.character(SF_7$NHS_No))

SF_7$NHS_No <- gsub("-","", as.character(SF_7$NHS_No))

SF_7$Hosp_No <- gsub(" ", "", as.character(SF_7$Hosp_No))


# Filtering out fields tat are no longer needed
SF_8 <- SF_7 %>% 
  select(- DoBDD, - DoBMD, - DoBYD, - DoBYD2, - YD, - MD, - DD, - ClinicDD, - ClinicMD, - ClinicYD, - ClinicYD2, - LetDD, - LetMD, - LetYD, - LetYD2)

#View(SF_8)


# 1b Diagnosis----

Diagnosis <- read.csv(file = "Diagnosis.csv",  sep = ",", na = c("","NA","null", "NULL"), header = FALSE)


# Adding the header

colnames(Diagnosis) <- c("Letter","Start","End","CUI","PREF","Negation","DiagCategory", "Certainty","Rule","DoBDD","DoBMD",	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode",
                         "Gender", "Address", "FirstName", "MiddleName", "Surname" )
 

# Creating a complete year out of two digit years under DoBYD2 i.e. 73 into 1973

Diagnosis_1 <- Diagnosis %>%
  mutate(DoBYD2 = case_when(DoBYD >1000 ~ as.numeric(DoBYD),
                            DoBYD <100 ~as.numeric(DoBYD+1900),
                            DoBYD < 20 ~ as.numeric(DoBYD+2000)), .after = "DoBYD")


# Creating a single date of date field

Diagnosis_2 <- Diagnosis_1 %>% 
  mutate(DoB = make_date(day = DoBDD, month = DoBMD, year = DoBYD2),.after = "DoBYD2" ) 

# For the NHS number verification we don't need to convert clinic and letter dates to single fields but we may as well do it as it would need to be done at some stage!

# clinic date
# same problem, year date may be written as yy so need to convert to yyyy under 
# ClinicYD2 and then make date


Diagnosis_3 <- Diagnosis_2 %>%
  mutate(ClinicYD2 = case_when(ClinicYD >1000 ~ as.numeric(ClinicYD),
                               ClinicYD <25 ~ as.numeric(ClinicYD+2000),
                               ClinicYD >25 ~ as.numeric(ClinicYD+1900)), .after = "ClinicYD")

Diagnosis_4 <- Diagnosis_3 %>% 
  mutate(Clinic_Date = make_date(day = ClinicDD, month = ClinicMD, year = ClinicYD2 ),.after = "ClinicYD2") 

# Letter date

Diagnosis_5 <- Diagnosis_4 %>%
  mutate(LetYD2 = case_when(LetYD >1000 ~ as.numeric(LetYD),
                            LetYD <25 ~ as.numeric(LetYD+2000),
                            LetYD >25 ~ as.numeric(LetYD+1900)), .after = "LetYD")


Diagnosis_6 <- Diagnosis_5 %>% 
  mutate(Let_Date = make_date(day = LetDD, month = LetMD, year = LetYD2 ),.after = "LetYD2") 

# creating record date = clinic date but if this is missing letter date

Diagnosis_7 <-Diagnosis_6 %>% 
  mutate(DATEREC = case_when(!is.na(Clinic_Date) ~ Clinic_Date,
                             is.na(Clinic_Date) & !is.na(Let_Date) ~ Let_Date),.after = "Let_Date")

#str(Diagnosis_7)

# removing spaces and "-" from NHS numbers ( for File 1 no spaces) and Hospital numbers

Diagnosis_7$NHS_No <- gsub(" ", "", as.character(Diagnosis_7$NHS_No))

Diagnosis_7$NHS_No <- gsub("-","", as.character(Diagnosis_7$NHS_No))

Diagnosis_7$Hosp_No <- gsub(" ", "", as.character(Diagnosis_7$Hosp_No))

#Removing fields that are no longer needed
Diagnosis_8 <- Diagnosis_7 %>% 
  select(- DoBDD, - DoBMD, - DoBYD, - DoBYD2, - ClinicDD, - ClinicMD, - ClinicYD, - ClinicYD2, - LetDD, - LetMD, - LetYD, - LetYD2)

#View(Diagnosis_8)


# 1c BirthHistory ----
BirthHistory <- read.csv(file = "BirthHistory.csv",  sep = ",",  na = c("","NA","null", "NULL"), header = FALSE)
# Adding the header
colnames(BirthHistory) <- c("Letter","Start","End","CUI","PREF","Negation","Experiencer", "Certainty","Rule","DoBDD","DoBMD",	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD",
                            "LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode","Gender", "Address", "FirstName", "MiddleName", "Surname" )

#View(BirthHistory)
# Creating a complete year out of two digit years under DoBYD2 i.e. 73 into 1973

BirthHistory_1 <- BirthHistory %>%
  mutate(DoBYD2 = case_when(DoBYD >1000 ~ as.numeric(DoBYD),
                            DoBYD <100 ~as.numeric(DoBYD+1900),
                            DoBYD < 20 ~ as.numeric(DoBYD+2000)), .after = "DoBYD")


# Creating a single date of date field

BirthHistory_2 <- BirthHistory_1 %>% 
  mutate(DoB = make_date(day = DoBDD, month = DoBMD, year = DoBYD2),.after = "DoBYD2" ) 

# For the NHS number verification we don't need to convert clinic and letter dates to single fields but we may as well do it as it would need to be done at some stage!

# clinic date
# same problem, year date may be written as yy so need to convert to yyyy under 
# ClinicYD2 and then make date


BirthHistory_3 <- BirthHistory_2 %>%
  mutate(ClinicYD2 = case_when(ClinicYD >1000 ~ as.numeric(ClinicYD),
                               ClinicYD <25 ~ as.numeric(ClinicYD+2000),
                               ClinicYD >25 ~ as.numeric(ClinicYD+1900)), .after = "ClinicYD")

BirthHistory_4 <- BirthHistory_3 %>% 
  mutate(Clinic_Date = make_date(day = ClinicDD, month = ClinicMD, year = ClinicYD2 ),.after = "ClinicYD2") 

# Letter date

BirthHistory_5 <- BirthHistory_4 %>%
  mutate(LetYD2 = case_when(LetYD >1000 ~ as.numeric(LetYD),
                            LetYD <25 ~ as.numeric(LetYD+2000),
                            LetYD >25 ~ as.numeric(LetYD+1900)), .after = "LetYD")


BirthHistory_6 <- BirthHistory_5 %>% 
  mutate(Let_Date = make_date(day = LetDD, month = LetMD, year = LetYD2 ),.after = "LetYD2") 

# creating record date = clinic date but if this is missing letter date

BirthHistory_7 <-BirthHistory_6 %>% 
  mutate(DATEREC = case_when(!is.na(Clinic_Date) ~ Clinic_Date,
                             is.na(Clinic_Date) & !is.na(Let_Date) ~ Let_Date),.after = "Let_Date")

str(Diagnosis_7)

# removing spaces and "-" from NHS numbers ( for File 1 no spaces) and Hospital numbers

BirthHistory_7$NHS_No <- gsub(" ", "", as.character(BirthHistory_7$NHS_No))

BirthHistory_7$NHS_No <- gsub("-","", as.character(BirthHistory_7$NHS_No))

BirthHistory_7$Hosp_No <- gsub(" ", "", as.character(BirthHistory_7$Hosp_No))

#Removing fields that are no longer needed
BirthHistory_8 <- BirthHistory_7 %>% 
  select(- DoBDD, - DoBMD, - DoBYD, - DoBYD2, - ClinicDD, - ClinicMD, - ClinicYD, - ClinicYD2, - LetDD, - LetMD, - LetYD, - LetYD2)

#View(BirthHistory_8)



# 1d EpilepsyCause----
EpiCause <- read.csv(file = "EpilepsyCause.csv",  sep = ",", na = c("","NA","null", "NULL"), header = FALSE)
# Adding the header
colnames(EpiCause) <- c("Letter","Start","End","CUI","PREF","Negation","Experiencer", "Certainty","Rule","DoBDD","DoBMD",	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD",
                        "LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode","Gender", "Address", "FirstName", "MiddleName", "Surname" )
#View(EpiCause)

EpiCause_1 <- EpiCause %>%
  mutate(DoBYD2 = case_when(DoBYD >1000 ~ as.numeric(DoBYD),
                            DoBYD <100 ~as.numeric(DoBYD+1900),
                            DoBYD < 20 ~ as.numeric(DoBYD+2000)), .after = "DoBYD")


# Creating a single date of date field

EpiCause_2 <- EpiCause_1 %>% 
  mutate(DoB = make_date(day = DoBDD, month = DoBMD, year = DoBYD2),.after = "DoBYD2" ) 

# For the NHS number verification we don't need to convert clinic and letter dates to single fields but we may as well do it as it would need to be done at some stage!

# clinic date
# same problem, year date may be written as yy so need to convert to yyyy under 
# ClinicYD2 and then make date


EpiCause_3 <- EpiCause_2 %>%
  mutate(ClinicYD2 = case_when(ClinicYD >1000 ~ as.numeric(ClinicYD),
                               ClinicYD <25 ~ as.numeric(ClinicYD+2000),
                               ClinicYD >25 ~ as.numeric(ClinicYD+1900)), .after = "ClinicYD")

EpiCause_4 <- EpiCause_3 %>% 
  mutate(Clinic_Date = make_date(day = ClinicDD, month = ClinicMD, year = ClinicYD2 ),.after = "ClinicYD2") 

# Letter date

EpiCause_5 <- EpiCause_4 %>%
  mutate(LetYD2 = case_when(LetYD >1000 ~ as.numeric(LetYD),
                            LetYD <25 ~ as.numeric(LetYD+2000),
                            LetYD >25 ~ as.numeric(LetYD+1900)), .after = "LetYD")


EpiCause_6 <- EpiCause_5 %>% 
  mutate(Let_Date = make_date(day = LetDD, month = LetMD, year = LetYD2 ),.after = "LetYD2") 

# creating record date = clinic date but if this is missing letter date

EpiCause_7 <-EpiCause_6 %>% 
  mutate(DATEREC = case_when(!is.na(Clinic_Date) ~ Clinic_Date,
                             is.na(Clinic_Date) & !is.na(Let_Date) ~ Let_Date),.after = "Let_Date")

#str(EpiCause_7)

# removing spaces and "-" from NHS numbers ( for File 1 no spaces) and Hospital numbers

EpiCause_7$NHS_No <- gsub(" ", "", as.character(EpiCause_7$NHS_No))

EpiCause_7$NHS_No <- gsub("-","", as.character(EpiCause_7$NHS_No))

EpiCause_7$Hosp_No <- gsub(" ", "", as.character(EpiCause_7$Hosp_No))

#Removing fields that are no longer needed
EpiCause_8 <- EpiCause_7 %>% 
  select(- DoBDD, - DoBMD, - DoBYD, - DoBYD2, - ClinicDD, - ClinicMD, - ClinicYD, - ClinicYD2, - LetDD, - LetMD, - LetYD, - LetYD2)

#View(EpiCause_8)



# 1e investigations----
Investigations <- read.csv(file = "Investigations.csv", sep = ",", na = c("","NA","null", "NULL"), header = FALSE)
# Adding the header
colnames(Investigations) <- c("Letter","Start","End","CUI","CT_Performed","CT_Results",
      "MRI_Performed","MRI_Results","EEG_Performed","EEG_Type",
      "EEG_Results", "Certainty","Negation","Rule","DoBDD","DoBMD",	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode",
      "Gender", "Address", "FirstName", "MiddleName", "Surname" )
#View(Investigations)

Investigations_1 <- Investigations %>%
  mutate(DoBYD2 = case_when(DoBYD >1000 ~ as.numeric(DoBYD),
                            DoBYD <100 ~as.numeric(DoBYD+1900),
                            DoBYD < 20 ~ as.numeric(DoBYD+2000)), .after = "DoBYD")


# Creating a single date of date field

Investigations_2 <- Investigations_1 %>% 
  mutate(DoB = make_date(day = DoBDD, month = DoBMD, year = DoBYD2),.after = "DoBYD2" ) 

# For the NHS number verification we don't need to convert clinic and letter dates to single fields but we may as well do it as it would need to be done at some stage!

# clinic date

Investigations_3 <- Investigations_2 %>%
  mutate(ClinicYD2 = case_when(ClinicYD >1000 ~ as.numeric(ClinicYD),
                               ClinicYD <25 ~ as.numeric(ClinicYD+2000),
                               ClinicYD >25 ~ as.numeric(ClinicYD+1900)), .after = "ClinicYD")

Investigations_4 <- Investigations_3 %>% 
  mutate(Clinic_Date = make_date(day = ClinicDD, month = ClinicMD, year = ClinicYD2 ),.after = "ClinicYD2") 

# Letter date

Investigations_5 <- Investigations_4 %>%
  mutate(LetYD2 = case_when(LetYD >1000 ~ as.numeric(LetYD),
                            LetYD <25 ~ as.numeric(LetYD+2000),
                            LetYD >25 ~ as.numeric(LetYD+1900)), .after = "LetYD")


Investigations_6 <- Investigations_5 %>% 
  mutate(Let_Date = make_date(day = LetDD, month = LetMD, year = LetYD2 ),.after = "LetYD2") 

# creating record date = clinic date but if this is missing letter date

Investigations_7 <-Investigations_6 %>% 
  mutate(DATEREC = case_when(!is.na(Clinic_Date) ~ Clinic_Date,
                             is.na(Clinic_Date) & !is.na(Let_Date) ~ Let_Date),.after = "Let_Date")

#str(Investigations_7)

# removing spaces and "-" from NHS numbers ( for File 1 no spaces) and Hospital numbers

Investigations_7$NHS_No <- gsub(" ", "", as.character(Investigations_7$NHS_No))

Investigations_7$NHS_No <- gsub("-","", as.character(Investigations_7$NHS_No))

Investigations_7$Hosp_No <- gsub(" ", "", as.character(Investigations_7$Hosp_No))

#Removing fields that are no longer needed
Investigations_8 <- Investigations_7 %>% 
  select(- DoBDD, - DoBMD, - DoBYD, - DoBYD2, - ClinicDD, - ClinicMD, - ClinicYD, - ClinicYD2, - LetDD, - LetMD, - LetYD, - LetYD2)

#View(Investigations_8)


# 1f Onset---- to be redone

Onset <- read.csv(file = "Onset.csv",  sep = ",", na = c("","NA","null", "NULL"), header = FALSE)

# Adding the header there was clinicYD was missing from groovy so will have to redo it
colnames(Onset) <- c("Letter","Start","End","CUI","PREF", "TimePeriod", "NumberOfTimePeriods",
                     "LowerNumberOfTimePeriods","UpperNumberOfTimePeriods",
  "YearDate","MonthDate","DayDate","PointInTime","Age","AgeLower","AgeUpper","AgeUnit",
  "Certainty","Negation","Experiencer","Rule", "DoBDD","DoBMD","DoBYD",
  "ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode",
  "Gender", "Address", "FirstName", "MiddleName", "Surname" )

#View(Onset)

Onset_1 <- Onset %>%
  mutate(DoBYD2 = case_when(DoBYD >1000 ~ as.numeric(DoBYD),
                            DoBYD <100 ~as.numeric(DoBYD+1900),
                            DoBYD < 20 ~ as.numeric(DoBYD+2000)), .after = "DoBYD")


# Creating a single date of date field

Onset_2 <- Onset_1 %>% 
  mutate(DoB = make_date(day = DoBDD, month = DoBMD, year = DoBYD2),.after = "DoBYD2" ) 

# For the NHS number verification we don't need to convert clinic and letter dates to single fields but we may as well do it as it would need to be done at some stage!

# clinic date
# same problem, year date may be written as yy so need to convert to yyyy under 
# ClinicYD2 and then make date


Onset_3 <- Onset_2 %>%
  mutate(ClinicYD2 = case_when(ClinicYD >1000 ~ as.numeric(ClinicYD),
                               ClinicYD <25 ~ as.numeric(ClinicYD+2000),
                               ClinicYD >25 ~ as.numeric(ClinicYD+1900)), .after = "ClinicYD")

Onset_4 <- Onset_3 %>% 
  mutate(Clinic_Date = make_date(day = ClinicDD, month = ClinicMD, year = ClinicYD2 ),.after = "ClinicYD2") 

# Letter date

Onset_5 <- Onset_4 %>%
  mutate(LetYD2 = case_when(LetYD >1000 ~ as.numeric(LetYD),
                            LetYD <25 ~ as.numeric(LetYD+2000),
                            LetYD >25 ~ as.numeric(LetYD+1900)), .after = "LetYD")


Onset_6 <- Onset_5 %>% 
  mutate(Let_Date = make_date(day = LetDD, month = LetMD, year = LetYD2 ),.after = "LetYD2") 

# creating record date = clinic date but if this is missing letter date

Onset_7 <-Onset_6 %>% 
  mutate(DATEREC = case_when(!is.na(Clinic_Date) ~ Clinic_Date,
                             is.na(Clinic_Date) & !is.na(Let_Date) ~ Let_Date),.after = "Let_Date")

#str(Onset_7)

# removing spaces and "-" from NHS numbers ( for File 1 no spaces) and Hospital numbers

Onset_7$NHS_No <- gsub(" ", "", as.character(Onset_7$NHS_No))

Onset_7$NHS_No <- gsub("-","", as.character(Onset_7$NHS_No))

Onset_7$Hosp_No <- gsub(" ", "", as.character(Onset_7$Hosp_No))

#Removing fields that are no longer needed
Onset_8 <- Onset_7 %>% 
  select(- DoBDD, - DoBMD, - DoBYD, - DoBYD2, - ClinicDD, - ClinicMD, - ClinicYD, - ClinicYD2, - LetDD, - LetMD, - LetYD, - LetYD2)

#View(Onset_8)


# 1g Prescriptions----

Prescription <- read.csv(file = "Prescription.csv", sep = ",", na = c("","NA","null", "NULL"), header = FALSE)
# Adding the header   For some reason DoBDD and DoBMD were missing from the output - checked groovy they are there - removing for a moment so I can get on!
colnames(Prescription) <- c("Letter","Start","End","CUI","DrugName","DrugDose","DoseUnit","Frequency","String","Rule","DoBDD","DoBMD",
                             	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode",
                            "Gender", "Address", "FirstName", "MiddleName", "Surname" )
Prescription_1 <- Prescription %>%
  mutate(DoBYD2 = case_when(DoBYD >1000 ~ as.numeric(DoBYD),
                            DoBYD <100 ~as.numeric(DoBYD+1900),
                            DoBYD < 20 ~ as.numeric(DoBYD+2000)), .after = "DoBYD")


# Creating a single date of date field

Prescription_2 <- Prescription_1 %>% 
  mutate(DoB = make_date(day = DoBDD, month = DoBMD, year = DoBYD2),.after = "DoBYD2" ) 

# For the NHS number verification we don't need to convert clinic and letter dates to single fields but we may as well do it as it would need to be done at some stage!

# clinic date
# same problem, year date may be written as yy so need to convert to yyyy under 
# ClinicYD2 and then make date


Prescription_3 <- Prescription_2 %>%
  mutate(ClinicYD2 = case_when(ClinicYD >1000 ~ as.numeric(ClinicYD),
                               ClinicYD <25 ~ as.numeric(ClinicYD+2000),
                               ClinicYD >25 ~ as.numeric(ClinicYD+1900)), .after = "ClinicYD")

Prescription_4 <- Prescription_3 %>% 
  mutate(Clinic_Date = make_date(day = ClinicDD, month = ClinicMD, year = ClinicYD2 ),.after = "ClinicYD2") 

# Letter date

Prescription_5 <- Prescription_4 %>%
  mutate(LetYD2 = case_when(LetYD >1000 ~ as.numeric(LetYD),
                            LetYD <25 ~ as.numeric(LetYD+2000),
                            LetYD >25 ~ as.numeric(LetYD+1900)), .after = "LetYD")


Prescription_6 <- Prescription_5 %>% 
  mutate(Let_Date = make_date(day = LetDD, month = LetMD, year = LetYD2 ),.after = "LetYD2") 

# creating record date = clinic date but if this is missing letter date

Prescription_7 <-Prescription_6 %>% 
  mutate(DATEREC = case_when(!is.na(Clinic_Date) ~ Clinic_Date,
                             is.na(Clinic_Date) & !is.na(Let_Date) ~ Let_Date),.after = "Let_Date")

#View(Prescription_7)

# removing spaces and "-" from NHS numbers ( for File 1 no spaces) and Hospital numbers

Prescription_7$NHS_No <- gsub(" ", "", as.character(Prescription_7$NHS_No))

Prescription_7$NHS_No <- gsub("-","", as.character(Prescription_7$NHS_No))

Prescription_7$Hosp_No <- gsub(" ", "", as.character(Prescription_7$Hosp_No))

#Removing fields that are no longer needed
Prescription_8 <- Prescription_7 %>% 
  select(- DoBDD, - DoBMD, - DoBYD, - DoBYD2, - ClinicDD, - ClinicMD, - ClinicYD, - ClinicYD2, - LetDD, - LetMD, - LetYD, - LetYD2)

#View(Prescription_8)



# 1h Patient history----
PH <- read.csv(file = "PatientHistory.csv",  sep = ",", na = c("","NA","null", "NULL"),header = FALSE)
# Adding the header
colnames(PH) <- c("Letter","Start","End","CUI","PREF", "TimePeriod", "NumberOfTimePeriods",
                     "LowerNumberOfTimePeriods","UpperNumberOfTimePeriods",
                     "YearDate","MonthDate","DayDate","PointInTime","Age","AgeLower","AgeUpper","AgeUnit",
                     "Certainty","Negation","Experiencer","Rule", "DoBDD","DoBMD","DoBYD",
                     "ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode",
                  "Gender", "Address", "FirstName", "MiddleName", "Surname" )

PH_1 <- PH %>%
  mutate(DoBYD2 = case_when(DoBYD >1000 ~ as.numeric(DoBYD),
                            DoBYD <100 ~as.numeric(DoBYD+1900),
                            DoBYD < 20 ~ as.numeric(DoBYD+2000)), .after = "DoBYD")


# Creating a single date of date field

PH_2 <- PH_1 %>% 
  mutate(DoB = make_date(day = DoBDD, month = DoBMD, year = DoBYD2),.after = "DoBYD2" ) 

# For the NHS number verification we don't need to convert clinic and letter dates to single fields but we may as well do it as it would need to be done at some stage!

# clinic date
# same problem, year date may be written as yy so need to convert to yyyy under 
# ClinicYD2 and then make date


PH_3 <- PH_2 %>%
  mutate(ClinicYD2 = case_when(ClinicYD >1000 ~ as.numeric(ClinicYD),
                               ClinicYD <25 ~ as.numeric(ClinicYD+2000),
                               ClinicYD >25 ~ as.numeric(ClinicYD+1900)), .after = "ClinicYD")

PH_4 <- PH_3 %>% 
  mutate(Clinic_Date = make_date(day = ClinicDD, month = ClinicMD, year = ClinicYD2 ),.after = "ClinicYD2") 

# Letter date

PH_5 <- PH_4 %>%
  mutate(LetYD2 = case_when(LetYD >1000 ~ as.numeric(LetYD),
                            LetYD <25 ~ as.numeric(LetYD+2000),
                            LetYD >25 ~ as.numeric(LetYD+1900)), .after = "LetYD")


PH_6 <- PH_5 %>% 
  mutate(Let_Date = make_date(day = LetDD, month = LetMD, year = LetYD2 ),.after = "LetYD2") 

# creating record date = clinic date but if this is missing letter date

PH_7 <-PH_6 %>% 
  mutate(DATEREC = case_when(!is.na(Clinic_Date) ~ Clinic_Date,
                             is.na(Clinic_Date) & !is.na(Let_Date) ~ Let_Date),.after = "Let_Date")

str(PH_7)

# removing spaces and "-" from NHS numbers ( for File 1 no spaces) and Hospital numbers

PH_7$NHS_No <- gsub(" ", "", as.character(PH_7$NHS_No))

PH_7$NHS_No <- gsub("-","", as.character(PH_7$NHS_No))

PH_7$Hosp_No <- gsub(" ", "", as.character(PH_7$Hosp_No))

#Removing fields that are no longer needed
PH_8 <- PH_7 %>% 
  select(- DoBDD, - DoBMD, - DoBYD, - DoBYD2, - ClinicDD, - ClinicMD, - ClinicYD, - ClinicYD2, - LetDD, - LetMD, - LetYD, - LetYD2)

#View(PH_8)

#View(PH_8)

WD <- read.csv(file = "WhenDiagnosed.csv",  sep = ",", na = c("","NA","null", "NULL"),header = FALSE)
# Adding the header
colnames(WD) <- c("Letter","Start","End","CUI","PREF", "TimePeriod", "NumberOfTimePeriods",
                  "LowerNumberOfTimePeriods","UpperNumberOfTimePeriods",
                  "YearDate","MonthDate","DayDate","PointInTime","Age","AgeLower","AgeUpper","AgeUnit",
                  "Certainty","Negation","Experiencer","Rule", "DoBDD","DoBMD","DoBYD",
                  "ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode",
                  "Gender", "Address", "FirstName", "MiddleName", "Surname" )

WD_1 <- WD %>%
  mutate(DoBYD2 = case_when(DoBYD >1000 ~ as.numeric(DoBYD),
                            DoBYD <100 ~as.numeric(DoBYD+1900),
                            DoBYD < 20 ~ as.numeric(DoBYD+2000)), .after = "DoBYD")


# Creating a single date of date field

WD_2 <- WD_1 %>% 
  mutate(DoB = make_date(day = DoBDD, month = DoBMD, year = DoBYD2),.after = "DoBYD2" )

# For the NHS number verification we don't need to convert clinic and letter dates to single fields but we may as well do it as it would need to be done at some stage!

# clinic date
# same problem, year date may be written as yy so need to convert to yyyy under 
# ClinicYD2 and then make date


WD_3 <- WD_2 %>%
  mutate(ClinicYD2 = case_when(ClinicYD >1000 ~ as.numeric(ClinicYD),
                               ClinicYD <25 ~ as.numeric(ClinicYD+2000),
                               ClinicYD >25 ~ as.numeric(ClinicYD+1900)), .after = "ClinicYD")

WD_4 <- WD_3 %>% 
  mutate(Clinic_Date = make_date(day = ClinicDD, month = ClinicMD, year = ClinicYD2 ),.after = "ClinicYD2") 

# Letter date

WD_5 <- WD_4 %>%
  mutate(LetYD2 = case_when(LetYD >1000 ~ as.numeric(LetYD),
                            LetYD <25 ~ as.numeric(LetYD+2000),
                            LetYD >25 ~ as.numeric(LetYD+1900)), .after = "LetYD")


WD_6 <- WD_5 %>% 
  mutate(Let_Date = make_date(day = LetDD, month = LetMD, year = LetYD2 ),.after = "LetYD2") 

# creating record date = clinic date but if this is missing letter date

WD_7 <-WD_6 %>% 
  mutate(DATEREC = case_when(!is.na(Clinic_Date) ~ Clinic_Date,
                             is.na(Clinic_Date) & !is.na(Let_Date) ~ Let_Date),.after = "Let_Date")

str(PH_7)

# removing spaces and "-" from NHS numbers ( for File 1 no spaces) and Hospital numbers

WD_7$NHS_No <- gsub(" ", "", as.character(WD_7$NHS_No))

WD_7$NHS_No <- gsub("-","", as.character(WD_7$NHS_No))

WD_7$Hosp_No <- gsub(" ", "", as.character(WD_7$Hosp_No))

#Removing fields that are no longer needed
WD_8 <- WD_7 %>% 
  select(- DoBDD, - DoBMD, - DoBYD, - DoBYD2, - ClinicDD, - ClinicMD, - ClinicYD, - ClinicYD2, - LetDD, - LetMD, - LetYD, - LetYD2)

#View(WD_8)




# Working out number of individuals - using seizure frequency, diagnosis, and patient history - at least one of these should be present in each letter

#simplifying names
SF_a <- SF_8 # seizure frequency
Diag_a <- Diagnosis_8
BH_a <- BirthHistory_8
EC_a <- EpiCause_8
Onset_a <- Onset_8
Inv_a <- Investigations_8
PH_a <- PH_8 # patient history
Presc_a <- Prescription_8
WD_a <- WD_8
# as surname may be written in allCAPS need to convert to upper or lower
SF_a$Surname <- toupper(SF_a$Surname) 
Diag_a$Surname <- toupper(Diag_a$Surname)
BH_a$Surname <- toupper(BH_a$Surname)
EC_a$Surname <- toupper(EC_a$Surname)
Onset_a$Surname <- toupper(Onset_a$Surname)
Inv_a$Surname <- toupper(Inv_a$Surname)
PH_a$Surname <- toupper(PH_a$Surname)
Presc_a$Surname <- toupper(Presc_a$Surname)
WD_a$Surname <- toupper(WD_a$Surname)

# selecting IDEx fields from the outputs

SF_b <- SF_a %>% 
  select(NHS_No, Surname, FirstName, DoB, Gender, PostCode,Address)

Diag_b <- Diag_a %>% 
  select(NHS_No, Surname, FirstName, DoB, Gender, PostCode, Address)

BH_b <- BH_a %>% 
  select(NHS_No, Surname, FirstName, DoB, Gender,PostCode, Address)

EC_b <- EC_a %>% 
  select(NHS_No, Surname, FirstName, DoB, Gender,PostCode, Address)
Onset_b <- Onset_a %>% 
  select(NHS_No, Surname, FirstName, DoB, Gender, PostCode, Address)
Inv_b <- Inv_a %>% 
  select(NHS_No, Surname, FirstName, DoB, Gender, PostCode, Address)
PH_b <- PH_a %>% 
  select(NHS_No, Surname, FirstName, DoB, Gender, PostCode, Address)
Presc_b <- Presc_a %>% 
  select(NHS_No, Surname, FirstName, DoB, Gender, PostCode, Address)
WD_b <- WD_a %>% 
  select(NHS_No, Surname, FirstName, DoB, Gender, PostCode, Address)

# this identifies Individuals in all outputs  with the address but this will be dropped when matching as there are many different forms of address

IND <- full_join(SF_b,Diag_b) %>% 
  full_join(BH_b) %>% 
  full_join(EC_b) %>% 
  full_join(Onset_b) %>% 
  full_join(Inv_b) %>% 
  full_join(PH_b) %>% 
  full_join(Presc_b) %>% 
  full_join(WD_b) %>% 
  distinct()
  
  

#View(IND)
# Dripping the Address field there is too match variation 
INDPC <- IND %>% 
  select(- Address) %>% 
  distinct()

# removing NA NHS numbers
INDNHS <- INDPC %>% 
  filter(!is.na(NHS_No))

# creating a separate column for NHS will allow for matching records from the outputs 
  
INDNHS <- INDNHS %>% 
  rename(NHS1 = NHS_No) %>% 
  distinct()

View(INDNHS)

# This creates File_1 

# need to match first to allocate NHS if known in other records -  based on surname, first name, date of birth and gender 
# as address may be written differently in letters we are going to select the first record per NHS, Surname etc group.
# this would also give first record for each group if NHS is NA i.e. Surname, FirstName and DoB group

FILE1prep <- INDNHS %>% 
  mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                               Gender == "female" ~ 2,
                               is.na(Gender) ~ 9))
  format(FILE1prep$DoB, "%d.%m.%Y")
  
  FILE1prep <- FILE1prep %>% 
    filter(!is.na(DoB), !is.na(Surname))

 View(FILE1prep) 
 
 # Linking back to IND which contains address field, need to convert Gender here too!
 # once linked we keep only the first mention of the Address for each group of personal details 
 
 IND <- IND%>%
   mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                                Gender == "female" ~ 2,
                                is.na(Gender) ~ 9))
 
 FILE_1 <- full_join(FILE1prep, IND, by=c( 'FirstName' = 'FirstName','Surname' = 'Surname', 'DoB' = 'DoB', 'GENDER_CD' = 'GENDER_CD', "PostCode" = "PostCode")) %>% 
 group_by(NHS1, Surname, FirstName, DoB, GENDER_CD, PostCode ) %>% 
 filter(row_number()==1) %>%  # selecting the 1st row of grouped (per person)records to get a single version of the address for a given postcode
   select(- Gender.y, - Gender.x) %>% 
   filter(!is.na(DoB), !is.na(Surname)) %>% 
   rename(NHS_NUMBER = NHS1, SURNAME = Surname, FORENAME = FirstName,  POSTCODE = PostCode,
          DATE_OF_BIRTH = DoB, ADDRESS_1 = Address) 
   
 
 #removing commas from the Address field as required for file_1'
 
 FILE_1$ADDRESS_1 <- gsub(",","", as.character(FILE_1$ADDRESS_1))
 
# Allocating SYSTEM_ID
 
 FILE_1$SYSTEM_ID <-1:nrow(FILE_1)
 
 FILE_1 <- FILE_1 %>% 
 mutate(CREATE_DATE = Sys.Date()) 
 
 
 # as postcode was used in grouping individuals some people can have more than one SYSTEM_ID (as thay changed address)
# they will still have the same NHS number so the assumption is that they will get the same ALF_PE within sail so they can be linked
 
 
 # Linking FILE_1 to all the outputs will assign NHS number when missing and the SYSTEM_ID to the records 


 # Seizure frequency 

# some output values conversions first
SF_a <- SF_a %>% 
  mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                               Gender == "female" ~ 2,
                               is.na(Gender) ~ 9))

           
SF_Matched <- left_join(SF_a, FILE_1, by=c('FirstName' = 'FORENAME','Surname' = 'SURNAME', 'DoB' = 'DATE_OF_BIRTH', 'PostCode' = 'POSTCODE', "GENDER_CD" = "GENDER_CD") ) 

View(SF_Matched)

           
# doing the same for diagnosis

Diag_a <- Diag_a %>% 
  mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                               Gender == "female" ~ 2,
                               is.na(Gender) ~ 9))


Diag_Matched <- left_join(Diag_a, FILE_1, by=c('FirstName' = 'FORENAME','Surname' = 'SURNAME', 'DoB' = 'DATE_OF_BIRTH', 'PostCode' = 'POSTCODE',"GENDER_CD" = "GENDER_CD") ) 

View(Diag_Matched)


# For Birth History
 
BH_a <- BH_a %>% 
  mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                               Gender == "female" ~ 2,
                               is.na(Gender) ~ 9))

BH_Matched <- left_join(BH_a, FILE_1, by=c('FirstName' = 'FORENAME','Surname' = 'SURNAME', 'DoB' = 'DATE_OF_BIRTH', 'PostCode' = 'POSTCODE',"GENDER_CD" = "GENDER_CD") ) 

View(BH_Matched)

# Epilepsy Cause

EC_a <- EC_a %>% 
  mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                               Gender == "female" ~ 2,
                               is.na(Gender) ~ 9))


EC_Matched <- left_join(EC_a, FILE_1, by=c('FirstName' = 'FORENAME','Surname' = 'SURNAME', 'DoB' = 'DATE_OF_BIRTH', 'PostCode' = 'POSTCODE',"GENDER_CD" = "GENDER_CD") ) 

View(EC_Matched)

# for Onset 

Onset_a <- Onset_a %>% 
  mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                               Gender == "female" ~ 2,
                               is.na(Gender) ~ 9))

Onset_Matched <- left_join(Onset_a, FILE_1, by=c('FirstName' = 'FORENAME','Surname' = 'SURNAME', 'DoB' = 'DATE_OF_BIRTH', 'PostCode' = 'POSTCODE',"GENDER_CD" = "GENDER_CD") ) 

View(Onset_Matched)

# For Investigations

Inv_a <- Inv_a %>% 
  mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                               Gender == "female" ~ 2,
                               is.na(Gender) ~ 9))

Inv_Matched <- left_join(Inv_a, FILE_1, by=c('FirstName' = 'FORENAME','Surname' = 'SURNAME', 'DoB' = 'DATE_OF_BIRTH', 'PostCode' = 'POSTCODE',"GENDER_CD" = "GENDER_CD") ) 

View(Inv_Matched)

# for patient history

PH_a <- PH_a %>% 
  mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                               Gender == "female" ~ 2,
                               is.na(Gender) ~ 9))

PH_Matched <- left_join(PH_a, FILE_1, by=c('FirstName' = 'FORENAME','Surname' = 'SURNAME', 'DoB' = 'DATE_OF_BIRTH', 'PostCode' = 'POSTCODE',"GENDER_CD" = "GENDER_CD") ) 

View(PH_Matched)


# For Prescription

Presc_a$Surname <- toupper(Presc_a$Surname)
Presc_a <- Presc_a %>% 
  mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                               Gender == "female" ~ 2,
                               is.na(Gender) ~ 9))

Presc_Matched <- left_join(Presc_a, FILE_1, by=c('FirstName' = 'FORENAME','Surname' = 'SURNAME', 'DoB' = 'DATE_OF_BIRTH', 'PostCode' = 'POSTCODE',"GENDER_CD" = "GENDER_CD") ) 

View(Presc_Matched)

 # For When Diagnosed

 WD_a <- WD_a %>% 
   mutate(GENDER_CD = case_when(Gender == "male" ~ 1,
                                Gender == "female" ~ 2,
                                is.na(Gender) ~ 9))
 
 WD_Matched <- left_join(WD_a, FILE_1, by=c('FirstName' = 'FORENAME','Surname' = 'SURNAME', 'DoB' = 'DATE_OF_BIRTH', 'PostCode' = 'POSTCODE',"GENDER_CD" = "GENDER_CD") ) 
 
 View(WD_Matched)
 
 
 
 
# Dissociative seizures - extraction from Patient History output
 DisSeiz <- PH_Matched %>% 
   filter(CUI == 'C3495874'|CUI == 'C1142430'|CUI == 'C0349245') %>% # 104 montions
   filter(Certainty > 3)  # 64 certainty 4 and 5 
 
#This uses the System ID asigned with a very strict match including postcode so individuals who have moved may be counted twice
 
 DisSeiz_Ind <- DisSeiz %>% 
   distinct(SYSTEM_ID)
 
 View(DisSeiz_Ind)
