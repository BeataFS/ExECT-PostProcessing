#Prescription output from ExECT precessing

library(readr)
library(sqldf)
library(RSQLite)
library(proto)
library(gsubfn)
library(dplyr)
library(tibble)

setwd("C:/Users/Beata/Documents/Epi25letters/ExECTOutput")
#Created new DOC column for all the letters so this will be used as a link when loaded to SAIL
#This was done in Excel on Epi25_LetterList.csv

Epi25_Clin_Let_Date <- read_delim("Epi25_Clin_Let_Date.csv", delim = ":", escape_double = FALSE, trim_ws = TRUE)
Epi25_LetterList <- read_delim("Epi25_LetterList.csv", delim = ":", escape_double = FALSE, trim_ws = TRUE)

View(Epi25_Clin_Let_Date)
View(Epi25_LetterList)
#1 Joinig clinic_Let_Date to LetterList to capture the new letter ref "DOC" ----


Epi25_Clin_Let_Date_DOC <- left_join(Epi25_Clin_Let_Date,
                  select(Epi25_LetterList,c(- SYSTEM_ID)), by="LETTER")
 
View(Epi25_Clin_Let_Date_DOC) 

 #saving Epi25_clin_Let_Date_DOC.csv as it it going to be used a lot
 write.csv(Epi25_Clin_Let_Date_DOC,file="Epi25_Clin_Let_Date_DOC.csv", row.names = TRUE)

 # checking how many letters had no clinic  or letter date = 8
NoDate <- anti_join(Epi25_LetterList, Epi25_Clin_Let_Date_DOC, by="DOC")
View(NoDate)

UniqueLet <- distinct(Epi25_Clin_Let_Date, LETTER)
View(UniqueLet) #there are 763 distinct docs with clinic/letter date

write.csv(Epi25_Date,file="Epi25_Date.csv", row.names = TRUE)

#2 Creating record date as DateRec ----
#which is a clinic date or letter date if clinic date is null

Epi25_RecDate_DOC <- Epi25_Clin_Let_Date_DOC %>% mutate(DateRec = case_when(!is.na(Clinic_Date) ~ Clinic_Date,#when clinic date is not null put clinic date as Date
                                          is.na(Clinic_Date) ~ LetDate), .after = "LetDate") #if clinic date is null put LetDate as Date, place date after LetDate
#converting Date to a proper date format and capital Y is for full year but r format is yyyy/mm/dd 
Epi25_RecDate_DOC$DateRec <- as.Date(Epi25_RecDate_DOC$DateRec, "%d/%m/%Y")

View(Epi25_RecDate_DOC)

#saving Epi25_Date as csv so it can be used for other analysis
write.csv(Epi25_RecDate_DOC,file="Epi25_RecDate_DOC.csv", row.names = TRUE)

# 3 Prescriptions and dates ----

Epi25_Prescription <- read_delim("Epi25_Prescriptions.csv", delim = ":", escape_double = FALSE, trim_ws = TRUE)
View(Epi25_Prescription)

Epi25_Prescriptions <- Epi25_Prescription %>% 
  rename(LETTER = Letter) #renaming column Letter as it was in the original output to LETTER

View(Epi25_Prescriptions)
#Linking ExECT Prescriptions output with Clin_Let_Date to get Record Date and DOC
#This would be the file to load into SAIL

Epi25_RecDate_DOC <- read_delim("Epi25_RecDate_DOC.csv", delim = ":", escape_double = FALSE, trim_ws = TRUE)
View(Epi25_RecDate_DOC)



Epi25_PRESC  <- left_join(Epi25_Prescriptions,
                            Epi25_RecDate_DOC %>% select(- SYSTEM_ID, - Clinic_Date, -LetDate),
                            by = "LETTER") 

View(Epi25_PRESC)
#making the final selection , order  and upper case for field names for the SAIL upload

Epi25_PRESC_DOC <- Epi25_PRESC %>% select(SYSTEM_ID, DATEREC, DOC,Start,End, CUI, Name, Dose, Unit, Frequency)
    names (Epi25_PRESC_DOC)  <- toupper (names(Epi25_PRESC_DOC))
Epi25_PRESC_DOC$DOSE <- as.numeric(Epi25_PRESC_DOC$DOSE,decimals = NULL)#converting DOSE to nemeric
Epi25_PRESC_DOC$FREQUENCY <- as.numeric(Epi25_PRESC_DOC$FREQUENCY, decimals = NULL)#converting Frequency to nemeric

    
View(Epi25_PRESC_DOC)
write.csv(Epi25_PRESC_DOC,file="Epi25_PRESC_DOC.csv", row.names = TRUE)
#From here on analysis that can be done in the Sailgate 

#Creating Quantity_mg for all doses expressed in mg

Prescriptions <- Epi25_PRESC_DOC %>% mutate(Quantity_mg = case_when(UNIT == "mg" ~ DOSE,
                                                           UNIT == "g" ~ DOSE*1000),.after = "UNIT" )

#4 Combining daily dose ----
#creating a subset of prescriptions output with a combined daily dose, first converting quantity to mg for all doses

View(Prescriptions)   #just checking...

#Total daily dose is Frequency * Dose and is called DailyDose and this can be calculetad
#But there are cases when a dose is expressed as 2 or even 3 instances of single frequency i.e. 
#Frequency is given as 1 more than once for the same ASM , We need to find these cases by matching letter (DOC) START (annotation start) and CUI 
#prescriptions should be sorted by "DOC" ,"Start" , CUI first

Prescriptions %>% 
        arrange(SYSTEM_ID, DATEREC, CUI, START, DOC)

#Find cases when DOSE is a second  or third dose of the same drug - create AnotherDose column


Prescriptions <- Prescriptions %>% mutate (AnotherDose = case_when(DOC == lag(DOC) & START == lag(START) & START != lag(START, 2) & CUI == lag(CUI) & FREQUENCY == '1' & lag(FREQUENCY) == '1' ~ '2nd',
                            DOC == lag(DOC) & DOC == lag(DOC,2) & START == lag(START) & START == lag(START, 2) & CUI == lag(CUI) & CUI == lag(CUI, 2) & FREQUENCY == '1' & lag(FREQUENCY, 2) == '1'~ '3rd',
                            DOC == lag(DOC,2) & START == lag(START, 2) & START != lag(START) & FREQUENCY == '1' & lag(FREQUENCY, 2) == '1' ~ '2nd'), .after = "FREQUENCY")

#Extract values of the 2nd and 3rd dose in separate columns but in one row (for some reason adding up lag column references does not work)
Prescriptions <- Prescriptions %>% mutate(Q2ndDose = case_when(AnotherDose == '2nd' & START == lag(START) & CUI == lag(CUI) ~ lag(Quantity_mg), 
AnotherDose == '2nd' & START == lag(START,2) & CUI == lag(CUI,2) ~ lag(Quantity_mg, 2)), .after = "AnotherDose")
Prescriptions <- Prescriptions %>% mutate(Q3rdDose = case_when(AnotherDose == '2nd' & lead(AnotherDose == '3rd') ~ lead(Quantity_mg)), .after = "Q2ndDose")
 
# 5 DailyDose calculation ----

Prescriptions <- Prescriptions %>% mutate(DailyDose = case_when(FREQUENCY >1 ~ Quantity_mg*FREQUENCY, 
                 !is.na(Q3rdDose) & AnotherDose == '2nd' ~  Quantity_mg + Q2ndDose + Q3rdDose,             
                 is.na(Q3rdDose) & AnotherDose == '2nd' ~ Quantity_mg + Q2ndDose,
                is.na(AnotherDose) & is.na(Q2ndDose) & is.na(Q3rdDose) & 
                START != lead(START) & START != lead(START, 2)  & FREQUENCY == 1 ~ Quantity_mg,
                START == lead( START) & DOC != lead(DOC) & CUI != lead(CUI)  & FREQUENCY == 1 ~ Quantity_mg,
                is.na(FREQUENCY) & is.na(AnotherDose) & CUI == "C0055891" ~ Quantity_mg), .after = "Q3rdDose" )

View(Prescriptions) 


#converting brand names to generic names so some continuation can be seen

Prescriptions <- Prescriptions %>% 
  mutate(PREF = case_when(NAME == "Epilim" ~ "Sodium Valproate",
         NAME == "Epilim Chrono" ~ "Sodium Valproate" ,
         CUI == "C0591452" ~ "Sodium Valproate" ,
         CUI == "C0037567" ~ "Sodium Valproate" ,
         NAME == "Lamotrigine" ~ "Lamotrigine",
         NAME == "Lamictal" ~ "Lamotrigine",
         CUI == "C0064636"~ "Lamotrigine",
         NAME == "Levetiracetam" ~ "Levetiracetam",
         NAME == "Keppra" ~ "Levetiracetam",
         CUI == "C0377265" ~ "Levetiracetam",
         CUI == "C2725260" ~ "Eslicarbazepine",
         NAME == "Tegretol" ~ "Carbamazepine",
         NAME == "Carbamazepine" ~ "Carbamazepine",
         CUI == "C0360123" ~ "Carbamazepine",
         CUI == "C0700087" ~"Carbamazepine",
         NAME == "Phenytoin" ~ "Phenytoin",
         NAME == "Topiramate" ~ "Topiramate",
         CUI == "C0076829" ~ "Topiramate",
         NAME == "Topamax" ~ "Topiramate",
         NAME == "Topiramate" ~ "Topiramate",
         NAME == "Brivaracetam" ~ "Brivaracetam",
         NAME == "Clobazam" ~ "Clobazam",
         NAME == "Zonisamide" ~ "Zonisamide",
         NAME == "Lacosamide" ~ "Lacosamide" ,
         NAME == "Perampanel" ~ "Perampanel" ,
         CUI == "C2698764" ~ "Perampanel",
         CUI == "C0009011" ~ "Clonazepam",
         CUI == "C0060926" ~ "Gabapentin",
         CUI == "C0893761" ~ "Lacosamide",
         CUI == "C0700016" ~ "Primidone",
         CUI == "C2725260" ~ "Eslicarbazepine", 
         CUI == "C0657912"~ "Pregabalin",
         CUI == "C0026056" ~ "Midazolam" ))

#Prescription information may be given more than once in a clinic letter so here we extract  
#date, CUI and daily dose for each person

#selecting a subset with the Daily Dose
DailyPrescription <- select(Prescriptions,SYSTEM_ID, DATEREC, CUI, PREF, DailyDose)

DailyPrescriptionFull = na.omit(DailyPrescription) # removing NA values from DailyDose (from the double/triple doses)

DailyPrescriptionFinal =  unique(DailyPrescriptionFull) # removing duplicate records = which gives 962 records
  
#Creating a df of maximum dose for each drug (CUI) and date, slice keeps the max daily dose, removing any lower doses of the same drug, per group
DailyPrescriptionMax <- DailyPrescriptionFinal %>% group_by(SYSTEM_ID, DATEREC, CUI, PREF) %>% slice(which.max(DailyDose))

View(DailyPrescriptionMax)  #This is the final output that can be linked to seizure frequency for example

DailyPrescriptionMax$DATEREC <- as.Date(DailyPrescriptionMax$DATEREC, "%d/%m/%Y")   #making sure these are dates

 write.csv(DailyPrescriptionMax,file="DailyPrescriptionMaxPREF.csv", row.names = TRUE)


 
 
 
 Precription_per_person <- DailyPrescriptionMax %>% 
  group_by(SYSTEM_ID) %>% 
count(SYSTEM_ID)

View(Precription_per_person)


Count_per_ID <- Precription_per_person %>% 
  group_by(SYSTEM_ID) %>% 
         count(SYSTEM_ID)
View(Count_per_ID)


library(ggplot2)

Presc_P1 <- DailyPrescriptionMax %>% 
  filter(SYSTEM_ID == x)
View(Presc_P1)








