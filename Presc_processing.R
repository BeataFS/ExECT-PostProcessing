
#Precessing of Prescription output from ExECT 

library(readr)
library(sqldf)
library(RSQLite)
library(proto)
library(gsubfn)
library(dplyr)
library(tibble)

#DateRec has already been added to the original prescription output from ExECT. IDEx provided clinic and letter 
#dates and based on these DAREREC was extracted and added to Prescriptions 

#1 Load Epi25_PRESC_DOC.csv ----

Epi25_PRESC_DOC <- read_delim("Epi25_PRESC_DOC.csv", delim = ":", escape_double = FALSE, trim_ws = TRUE)


#2 Creating Quantity_mg for all doses expressed in mg

Prescriptions <- Epi25_PRESC_DOC %>% mutate(Quantity_mg = case_when(UNIT == "mg" ~ DOSE,
                                                                UNIT == "g" ~ DOSE*1000),.after = "UNIT" )

#3 Combining daily dose ----
#creating a subset of prescriptions output with a combined daily dose, first converting quantity to mg for all doses

View(Prescriptions)   #just checking...

#Total daily dose is Frequency * Dose and is called DailyDose and this can be calculetad
#But there are cases when a dose is expressed as 2 or even 3 instances of single frequency i.e. 
#Frequency is given as 1 more than once for the same ASM , We need to find these cases by matching letter (DOC) START (annotation start) and CUI 
#prescriptions should be sorted by "DOC" ,"Start" , CUI first

Prescriptions %>% 
  arrange(SYSTEM_ID, DATEREC, DOC, START, CUI)

#4 Multiple single doses ---- 
#Find cases when DOSE is a second  or third dose of the same drug - create AnotherDose column


Prescriptions <- Prescriptions %>% mutate (AnotherDose = case_when(DOC == lag(DOC) & START == lag(START) & START != lag(START, 2) & CUI == lag(CUI) & FREQUENCY == '1' & lag(FREQUENCY) == '1' ~ '2nd',
                                                                   DOC == lag(DOC) & DOC == lag(DOC,2) & START == lag(START) & START == lag(START, 2) & CUI == lag(CUI) & CUI == lag(CUI, 2) & FREQUENCY == '1' & lag(FREQUENCY, 2) == '1'~ '3rd',
                                                                   DOC == lag(DOC,2) & START == lag(START, 2) & START != lag(START) & FREQUENCY == '1' & lag(FREQUENCY, 2) == '1' ~ '2nd'), .after = "FREQUENCY")

#Extract values of the 2nd and 3rd dose in separate columns but in one row (for some reason adding up lag column references does not work)
Prescriptions <- Prescriptions %>% mutate(Q2ndDose = case_when(AnotherDose == '2nd' ~ lag(Quantity_mg)), .after = "AnotherDose")

Prescriptions <- Prescriptions %>% mutate(Q3rdDose = case_when(AnotherDose == '2nd' & lead(AnotherDose == '3rd') ~ lead(Quantity_mg)), .after = "Q2ndDose")

# 5 DailyDose calculation ----

Prescriptions <- Prescriptions %>% mutate(DailyDose = case_when(FREQUENCY >1 ~ Quantity_mg*FREQUENCY, 
                                                                !is.na(Q3rdDose) & AnotherDose == '2nd' ~  Quantity_mg + Q2ndDose + Q3rdDose,             
                                                                is.na(Q3rdDose) & AnotherDose == '2nd' ~ Quantity_mg + Q2ndDose,
                                                                is.na(AnotherDose) & is.na(Q2ndDose) & is.na(Q3rdDose) & 
                                                                  START != lead(START) & START != lead(START, 2)  & FREQUENCY == 1 ~ Quantity_mg,
                                                                START == lead( START) & DOC != lead(DOC) & CUI != lead(CUI)  & FREQUENCY == 1 ~ Quantity_mg,
                                                                is.na(FREQUENCY) & is.na(AnotherDose) & CUI == "C0055891" ~ Quantity_mg), .after = "Q3rdDose" )

View(DailyPrescriptionFinal) 

# 6 Removing duplicates ---- 
#Prescription information may be given more than once in a clinic letter so here we extract  
#date, CUI and daily dose for each person

#selecting a subset with the Daily Dose
DailyPrescription <- select(Prescriptions,SYSTEM_ID, DATEREC, DOC, CUI, NAME, DailyDose)

DailyPrescriptionFull = na.omit(DailyPrescription) # removing NA values from DailyDose (from the double/triple doses)

DailyPrescriptionFinal =  unique(DailyPrescriptionFull) # removing duplicate records = which gives 962 records

#7 Cleaning and finding Max dose ----
#Creating a df of maximum dose for each drug (CUI) and date, slice keeps the max daily dose, removing any lower doses of the same drug, per group
DailyPrescriptionMax <- DailyPrescriptionFinal %>% group_by(SYSTEM_ID, DATEREC, DOC, CUI) %>% slice(which.max(DailyDose))

View(DailyPrescriptionMax)  #This is the final output that can be linked to seizure frequency for example

write.csv(DailyPrescriptionMax,file="DailyPrescriptionMax.csv", row.names = TRUE)







