# ExECT precessing using of z drive output. As IDEx was run as part of the ExECT pipeline we need to include 
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



SF <- read.csv(file = "SeizureFrequency.csv", sep = ":", na = c("","NA","null", NULL), header = FALSE) # had to add sep ":" as this is how the file came out from GATE

# Adding headers, we may also need to concatenate clinic date, letter date, and DoB - later!

colnames(SF) <- c("Letter","Start","End","CUI","String","FreqChange",	"NofS",	"LNofS"	,"UNofS","TP",	"NofTP","LNofTP",	"UNofTP",	"Sin_Dur","YD",	"MD",	"DD",	"PinT",	"Age","AgeLower",	"AgeUpper",	"AgeUnit",	"Certainty",	"Negation",	"Rule",	"DoBDD",	"DoBMD",	"DoBYD"	,"ClinicDD",	"ClinicMD","ClinicYD",	"LetDD",	"LetMD",	"LetYD","Hosp_No","NHS_No","PostCode")

# 1b Diagnosis----

Diagnosis <- read.csv(file = "Diagnosis.csv", header = FALSE)

# Adding the header

colnames(Diagnosis) <- c("Letter","Start","End","CUI","PREF","Negation","DiagCategory", "Certainty","Rule","DoBDD","DoBMD",	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode","NA")

# 1c BirthHistory ----
BirthHistory <- read.csv(file = "BirthHistory.csv", header = FALSE)
# Adding the header
colnames(BirthHistory) <- c("Letter","Start","End","CUI","PREF","Negation","Experiencer", "Certainty","Rule","DoBDD","DoBMD",	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode")

# 1d EpilepsyCause----
EpiCause <- read.csv(file = "EpilepsyCause.csv", header = FALSE)
# Adding the header
colnames(EpiCause) <- c("Letter","Start","End","CUI","PREF","Negation","Experiencer", "Certainty","Rule","DoBDD","DoBMD",	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode")

# 1e investigations----
Investigations <- read.csv(file = "Investigations.csv",sep = ":", header = FALSE)
# Adding the header
colnames(Investigations) <- c("Letter","Start","End","CUI","CT_Performed","CT_Results",
      "MRI_Performed","MRI_Results","EEG_Performed","EEG_Type",
      "EEG_Results", "Negation","Rule","DoBDD","DoBMD",	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode")

# 1f Onset---- to be redone
Onset <- read.csv(file = "Onset.csv", header = FALSE)
# Adding the header there was clinicYD was missing from groovy so will have to redo it
colnames(Onset) <- c("Letter","Start","End","CUI","PREF", "TimePeriod", "NumberOfTimePeriods",
                     "LowerNumberOfTimePeriods","UpperNumberOfTimePeriods",
  "YearDate","MonthDate","DayDate","PointInTime","Age","AgeLower","AgeUpper","AgeUnit",
  "Certainty","Negation","Experiencer","Rule", "DoBDD","DoBMD","DoBYD",
  "ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode")



# 1g Prescriptions----

Prescription <- read.csv(file = "Prescription.csv",sep = ":", header = FALSE)
# Adding the header   For some reason DoBDD and DoBMD were missing from the output - checked groovy they are there - removing for a moment so I can get on!
colnames(Prescription) <- c("Letter","Start","End","CUI","DrugName","DrugDose","DoseUnit","Frequency","String","Rule",
                             	"DoBYD"	,"ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode")


# 1h Patient history----
PH <- read.csv(file = "PatientHistory.csv", header = FALSE)
# Adding the header
colnames(PH) <- c("Letter","Start","End","CUI","PREF", "TimePeriod", "NumberOfTimePeriods",
                     "LowerNumberOfTimePeriods","UpperNumberOfTimePeriods",
                     "YearDate","MonthDate","DayDate","PointInTime","Age","AgeLower","AgeUpper","AgeUnit",
                     "Certainty","Negation","Experiencer","Rule", "DoBDD","DoBMD","DoBYD",
                     "ClinicDD","ClinicMD","ClinicYD","LetDD","LetMD","LetYD","Hosp_No","NHS_No","PostCode", "NA")

head(PH,10)

# 1i Working out number of individuals

SF_NHS_NO <- distinct(SF, NHS_No)

# some experiments with naniar to replace null but it was all down to that space
# need to think about this later
# library(naniar)

# na_strings <- c("NA", " null") # there was a space !
# SF_NHS_No %>%
 #  replace_with_na_all(condition = ~.x %in% na_strings)

SF_NHS_NO <- SF_NHS_NO %>% 
filter(NHS_No != " null") # there was a space there !  this can most likely be done with "gsub()"



SF_NHS_NO$NHS_No<-gsub(" ", "", as.character(SF_NHS_NO$NHS_No))

SF_NHS_NO$NHS_No<-gsub("-","", as.character(SF_NHS_NO$NHS_No))


Diagnosis_NHS_NO <- distinct(Diagnosis, NHS_No) %>% 
  filter(NHS_No != " null")


 Diagnosis_NHS_NO$NHS_No<-gsub(" ", "", as.character(Diagnosis_NHS_NO$NHS_No))
 
 Diagnosis_NHS_NO$NHS_No<-gsub("-","", as.character(Diagnosis_NHS_NO$NHS_No))


PH_NHS_NO <-  distinct(PH, NHS_No) %>% 
filter(NHS_No != " null")
 


 PH_NHS_NO$NHS_No<-gsub(" ", "", as.character(PH_NHS_NO$NHS_No))
 
 PH_NHS_NO$NHS_No<-gsub("-","", as.character(PH_NHS_NO$NHS_No))
 
SF_DIAG_PH_NHS_NO <- full_join(SF_NHS_NO, Diagnosis_NHS_NO) %>% 
  full_join(PH_NHS_NO) %>% 
  distinct(NHS_No) %>% 
  count()

View(SF_DIAG_PH_NHS_NO)  #it gives a total number of people, 1 error noticed with a longer NHS number so it should be 108, but otherwise OK
