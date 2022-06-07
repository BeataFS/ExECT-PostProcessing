#Seizure frequency output from ExECT precessing

library(readr)
library(dplyr)
library(zoo) #for dates = but experymented here with origin
library(lubridate)
library(readxl)

options(scipen = 999) #numbers should not be displayed in scientific notation
options(digits = 6) #Number of digits to be displayed

setwd("C:/Users/Beata/Documents/Epi25letters/ExECTOutput")# set working dir

#create a file for SAIL changing letters to DOCs 
#ExECT seizure frequency output - Epi25_SF.csv

Epi25_SF <- read_delim("Epi25_SF.csv", delim = ":", escape_double = FALSE, trim_ws = TRUE) #seizure frequency
Epi25_RecDate_DOC <- read_delim("Epi25_RecDate_DOC.csv", delim = ":", escape_double = FALSE, trim_ws = TRUE) #letters with dates and DOC ids
View(Epi25_SF)
# Joinig Epi25_SF to RecDate_DOC to capture the new letter ref "DOC"
#Epi25_LetterList$Letter = toupper(Epi25_LetterList$Letter) #changed to upper as it is like this in other files 
Epi25_SF <- Epi25_SF %>% 
  rename(LETTER = Letter)

SF_DOC <- left_join(Epi25_SF,
                    select(Epi25_RecDate_DOC,c(- SYSTEM_ID)), by="LETTER") # Joining by letter
View(SF_DOC)

#final selection (without letter) and order - this produces file for SAIL
Epi25_SF_DOC <- select(SF_DOC, SYSTEM_ID, DATEREC, DOC,Start, End, CUI, FreqChange, NofS, LNofS, UNofS, TP, NofTP, LNofTP, UNofTP, Sin_Dur, YD, MD, DD, PinT) 
Epi25_SF_DOC$DATEREC <- as.Date(Epi25_SF_DOC$DATEREC, "%d/%m/%Y") #making sure that DATEREC is in date format
View(Epi25_SF_DOC)
# This file is for SAIL upload
write.csv(Epi25_SF_DOC,file="Epi25_SF_DOC.csv", row.names = TRUE)

write_excel_csv(Epi25_SF_DOC, file = "Epi25_SF_DOC") 

#This file is to be used outside of SAIL, contains letter references from GATE (ExECT , IDEx)
Epi25_SF_LETT <- select(SF_DOC, SYSTEM_ID, DATEREC, LETTER,Start, End, CUI, FreqChange, NofS, LNofS, UNofS, TP, NofTP, LNofTP, UNofTP, Sin_Dur, YD, MD, DD, PinT) 
Epi25_SF_LETT$DATEREC <- as.Date(Epi25_SF_LETT$DATEREC, "%d/%m/%Y") #making sure that DATEREC is in date format
write.csv(Epi25_SF_LETT,file="Epi25_SF_LETT.csv", row.names = TRUE)

View(Epi25_SF_LETT)
#Step by step analysis of seizure frequency output from ExECT. Based on the output from separate documents
#using DOC as reference. DateRec can be used instead to identify individual records, although for some letters this can be missing.

SF <- Epi25_SF_LETT
SF <- data.frame(SF)

View(SF)

SF$DATEREC <- as.factor(SF$DATEREC)
SF$DATEREC <- as.Date(SF$DATEREC, format = "%d/%m/%Y" )

# 1 number of seizures ---- 
# Creating a single column "NumS" from number of seizures (NofS) and Upper number of seizures (UNofS) 
#ignoring lower no of seizures as no values without the upper range, but considering cases with during as the rate for these is based on daysdiff

SF$NofS <- as.numeric(SF$NofS)
SF$UNofS <- as.numeric(SF$UNofS)

# Need to convert Sin_dur null values to NA as for some reason they come as null and the script does not work
# But it's much easier to specific NA fot null when reading in the data!
is.na(SF$Sin_Dur) <- SF$Sin_Dur == "null"



SF <- SF %>%  mutate(NumS = as.numeric(case_when((Sin_Dur == "Since" | is.na(Sin_Dur)) & !is.na(NofS) ~ NofS, # number of seizures which do not occur during a specified period
                                      (Sin_Dur == "Since" | is.na(Sin_Dur)) & is.na(NofS) & UNofS>0 ~ UNofS, #as above but with a range - using the upper number
                                      Sin_Dur == "During" & (TP != "Week"| is.na(TP)) & !is.na(NofS) ~ NofS, # seizures occur during a period of time but not weekly so date will be used to get daysdiff to calculate daily rate
                                      Sin_Dur == "During" & (TP != "Week" |is.na(TP)) & is.na(NofS) & UNofS>0 ~ UNofS, # as above but using the upper number of seizures from a range
                                      Sin_Dur == "During" & TP == "Week" & !is.na(MD) & !is.na(NofS) ~ 4.3*NofS, #when seizures occur during specific month for a number of weeks so we get overall number of episodes in that month 
                                      Sin_Dur == "During" & TP == "Week" & !is.na(MD) & is.na(NofS) & UNofS>0 ~ 4.3*UNofS ))) # as above but for the upper number in a range


#2 Number of time periods ----
# Creating a single column "NumTP" from number of time periods (NofTP) and  lower number of time periods (LNofTP) 
#ignoring upper number of time periods as we want the most frequent seizures from the final calculation
SF$NofTP <- as.numeric(SF$NofTP)
SF$LNofTP <- as.numeric(SF$LNofTP)
SF$UNofTP <- as.numeric(SF$UNofTP)

SF <- SF %>% mutate(NTP = as.numeric(case_when(
  NofTP>0 & !is.na(NofTP) ~ NofTP,
  is.na(NofTP) & LNofTP>0 ~ LNofTP
  )
))

# 3 Seizure frequency per day ----

#creates a column with all time periods as days, we are trying to compare seizure frequency per different time periods, 
#days would give a common denominator but it could be done in weeks or months.
SF <- SF %>% mutate(Days = case_when(TP == "Day" ~ 1,
                                     TP == "Week" ~ 7,
                                     TP == "Month" ~ 30,
                                     TP == "Year" ~ 365))


# calculating the total number of days in the time periods given in TP and NofTP


SF <- SF %>%  mutate(TotalTPDays = case_when(!is.na(Days) & !is.na(NTP) ~ Days*NTP))
                                             


# 3 Number of Seizures per Time Period ----
#adding a column SperTP which is the result of calculating the number of seizures per stated time period

#SF <- SF %>% mutate(SperTP = NumS/NumTP, .after = "NumTP") # as we are dong per day rate directly this is not needed

# 4 number of seizures per day ----

SF <- SF %>% mutate(DailyRate = case_when(is.na (Sin_Dur) ~ NumS/TotalTPDays)) 


View(SF)

#5 EventDate based on separate DD,MD, and YD columns ----

#converting numerical values given as days (DD) and months (MD) to two figire format
SF <- SF %>% mutate(DD = ifelse(!is.na(DD) & as.numeric(DD) < 10,paste("0", DD, sep = "") ,DD)) # convert from 9 to 09 for date

SF <- SF %>% mutate(MD = ifelse(!is.na(MD) & as.numeric(MD) < 10,paste("0", MD, sep = "") ,MD))

# if null values were not converted to NA wen loading csv do it now as this does not work otherwise
is.na(SF$YD) <- SF$YD == "null"

SF <- SF %>% mutate(EventDate = ifelse(!is.na(YD) & !is.na(MD) & !is.na(DD), paste(DD, MD, YD, sep = "/"),#All day month year there
                          ifelse(!is.na(YD) & !is.na(MD) & is.na(DD), paste("01", MD, YD, sep = "/"),## no day only month and year
                                 ifelse(!is.na(YD) & is.na(MD) & is.na(DD), paste("01","01", YD, sep = "/"),# year only
                                        ifelse(is.na(YD) & !is.na(MD) & is.na(DD), paste("01", MD, ifelse(as.numeric(MD) <  as.numeric(format(DATEREC, "%m")), format(DATEREC, "%Y"), as.character(as.numeric(format(DATEREC, "%Y"))-1)), sep = "/"), #month only - using DATEREC but it could be clinic or letter date, get year of or year before (if month mentioned is after month of DATEREC)
                                               ifelse(is.na(YD) & !is.na(MD) & !is.na(DD), paste(DD, MD, ifelse(as.numeric(MD) <  as.numeric(format(DATEREC, "%m")), format(DATEREC, "%Y"), as.character(as.numeric(format(DATEREC, "%Y")) -1)), sep = "/"), NA))))), .after = "DATEREC")#As above but with day also (in above set the 1st of month)

rlang::last_error()

#Creating EventDate2 for cases when there are no date values and event was in Point in time (PinT) "Last_Month" or "Last_Week" using DATEREC and some date calculations


SF <- SF %>% mutate(EventDate2  = case_when(is.na(YD) & is.na(MD) & is.na(DD) & PinT == "Last_Month" ~ DATEREC - 30, #when Point in time is last month we can take 30 days away from DATEREC
                                           is.na(YD) & is.na(MD) & is.na(DD) & PinT == "Last_Year" ~ DATEREC - 365), .after = "EventDate") #when Point in time is last year we can take 365 days away from DATEREC

SF$EventDate <- as.Date(SF$EventDate, "%d/%m/%Y")   
SF$EventDate2 <- as.Date(SF$EventDate2, "%d/%m/%Y")  



# 6 Previous record date and previous record for CUI ----

SF <- SF %>% arrange(SF, SYSTEM_ID, DATEREC, CUI) # ordering data by SYSTEM_ID and date of record first - really important as we are using functions that look back in time

SF <- SF %>% mutate(PrevRecDate = ifelse(SYSTEM_ID == lag(SYSTEM_ID) & DATEREC > lag(DATEREC), lag(DATEREC),
                                        NA ), .after = "DATEREC")
SF$PrevRecDate <- as.Date(SF$PrevRecDate, origin = "1970-01-01")


#PrevRecCUI finds a date of previous clinic - going back up to 6 rows in records sorted by DATEREC /CUI  for the particular CUI i.e. seizure type
SF <- SF %>%  mutate(PrevRecCUI = as.Date( ifelse(CUI == lag(CUI) & SYSTEM_ID == lag(SYSTEM_ID) & DATEREC > lag(DATEREC), lag(DATEREC),
                                                  ifelse(CUI == lag(CUI, 2) & SYSTEM_ID == lag(SYSTEM_ID, 2) & DATEREC > lag(DATEREC, 2), lag(DATEREC, 2),
                                                         ifelse(CUI == lag(CUI, 3) & SYSTEM_ID == lag(SYSTEM_ID, 3) & DATEREC > lag(DATEREC, 3) ,lag(DATEREC, 3),     
                                                                ifelse(CUI == lag(CUI, 4) & SYSTEM_ID == lag(SYSTEM_ID, 4) & DATEREC > lag(DATEREC, 4), lag(DATEREC, 4), 
                                                                       ifelse(CUI == lag(CUI, 5) & SYSTEM_ID == lag(SYSTEM_ID, 5)& DATEREC > lag(DATEREC, 5), lag(DATEREC, 5),
                                                                              ifelse(CUI == lag(CUI, 6) & SYSTEM_ID == lag(SYSTEM_ID, 6) & DATEREC > lag(DATEREC, 6), lag(DATEREC, 6), NA))))))),.after = "DATEREC" )

# 7 DATEREC as year only to be used to get an event date for point in time 'this year'

SF$DATEREC_Year <- as.Date(format(SF$DATEREC, '%Y/01/01'), .after = "DATEREC") # new field with year of RECDATE only

# 8 Difference in days for different Points in Time----
#difference in days between DATEREC and event date  to calculate number of seizures per day for that period

SF <- SF %>% mutate(DaysDiff=case_when(!is.na(EventDate) ~ DATEREC - EventDate, #this one uses event date created from dates 
                                       is.na(EventDate) & PinT == "LastClinic" & !is.na(PrevRecDate) ~ DATEREC - PrevRecDate, # this one uses previous clinic date no matter what CUI
                                       is.na(EventDate) & PinT == "Last_Month" |PinT == "Last_Year" ~ DATEREC - EventDate2, # this one uses event date based on point in time
                                      is.na(EventDate) & PinT == "This_Year" ~ DATEREC - DATEREC_Year, # this one uses Year from DATEREC to get the number of days between the beginning of the year and the DATEREC
                                      is.na(EventDate) & FreqChange == "Same" ~ DATEREC - PrevRecCUI)) # when frequency from the previous record for the same CUI is going to be used 

SF$DaysDiff <- as.numeric(SF$DaysDiff)

# 9  Daily Rates part 2  ----
#shows seizures per day reported as seizures since  (using dates) as DailyRate2

SF <- SF %>% mutate(DailyRate2 = case_when(!is.na(Sin_Dur) ~  NumS/DaysDiff, # when there is no clear seizures per time period but seizures during or since  
                                            PinT != "LastClinic" ~ NumS/DaysDiff,
                                           PinT == "LastClinic" & DaysDiff < 183 ~ NumS/DaysDiff)) # we are looking at time (days) since last clinic if it isn't longer than 6 months

SF <- SF %>% mutate(DailyRateF = case_when(!is.na(DailyRate) ~ DailyRate,
                                             is.na(DailyRate) ~ DailyRate2),.after = "DailyRate2")


# 10 Seizure free ----
#Shows number of days seizure free as based on 0 seizures for a number of days (calculated field) or 0 seizures in the number of days calculated from
#event day or last clinic

SF <- SF %>% mutate(SeizureFree = case_when(NumS == 0 & !is.na(Days) ~ TotalTPDays, #as the seizure per time period (SperTP) willalways be 0 for 0 seizures we need to take the original number of TP and multiply by days
                                              NumS == 0 & is.na(Days) ~ DaysDiff ))

View(SF)


#Using PrevRecCUI is only used to inspect the output as the dates are on the same line, the final script to extract
#frequency which was reported for a specific CUI in previous clinic creates PrevCUIFreq

SF <- SF %>%  mutate(PrevCUIFreq = case_when(SYSTEM_ID == lag(SYSTEM_ID) & CUI == lag(CUI) & DATEREC > lag(DATEREC) & is.na(DailyRateF) ~ lag(DailyRateF),
                                             SYSTEM_ID == lag(SYSTEM_ID, 2) &  CUI == lag(CUI, 2) & DATEREC > lag(DATEREC, 2) & is.na(DailyRateF) ~ lag(DailyRateF, 2),
                                             SYSTEM_ID == lag(SYSTEM_ID, 3) &  CUI == lag(CUI, 3) & DATEREC > lag(DATEREC, 3) & is.na(DailyRateF) ~ lag(DailyRateF, 3),     
                                             SYSTEM_ID == lag(SYSTEM_ID, 4) &  CUI == lag(CUI, 4) & DATEREC > lag(DATEREC, 4) & is.na(DailyRateF) ~ lag(DailyRateF, 4), 
                                             SYSTEM_ID == lag(SYSTEM_ID, 5) &  CUI == lag(CUI, 5) & DATEREC > lag(DATEREC, 5) & is.na(DailyRateF) ~ lag(DailyRateF, 5),
                                             SYSTEM_ID == lag(SYSTEM_ID, 6) &  CUI == lag(CUI, 6) & DATEREC > lag(DATEREC, 6) & is.na(DailyRateF) ~ lag(DailyRateF, 6)),.after = "DailyRateF" )


#11 "Same" in FreqChange ----
#if seizure frequency is reported as "same"  in FreqChange find a letter that is the most recent to the date of "Same" record and use PrevCUIFreq for that record

SF <- SF %>% mutate(PrevRateCUI_Same6m = case_when(FreqChange == "Same" & DaysDiff < 183 ~ PrevCUIFreq))#rather than using PrevRecDate we are using specific CUI previous date

#Adding previous < 6m CUI specific seizure frequency to daily rate 

#12 Daily rates take 3
SF <- SF %>% mutate(DailyRateSF = case_when(!is.na(DailyRateF) ~ DailyRateF,
                                           is.na(DailyRateF) ~ PrevRateCUI_Same6m))


#13Adding frequency scores ----
#Frequency score calculated as per day rate with seizure free given priority 
#using the calculated columns of daily rate (DailyRateSF final rate and rate for Same frequency under FreqChange) 


SF <- SF %>% mutate(FreqSeverity = case_when(SeizureFree < 7 ~ 6,
                                               SeizureFree > 6 & SeizureFree < 30 ~ 5,
                                               SeizureFree > 29 & SeizureFree < 182 ~ 4,
                                               SeizureFree > 181 & SeizureFree < 365 ~ 3,
                                               SeizureFree > 364 & SeizureFree < 730 ~ 2,
                                               SeizureFree > 729 ~ 1,
                                               DailyRateSF > 1 ~ 7,
                                               DailyRateSF > 0.2857143 &  DailyRateSF < 2 ~ 6,
                                               DailyRateSF > 0.0657534 &  DailyRateSF < 0.2857144 ~ 5,
                                               DailyRateSF > 0.0109589 &  DailyRateSF < 0.0657535 ~ 4,
                                               DailyRateSF > 0.0054795 &  DailyRateSF < 0.0109590 ~ 3,
                                               DailyRateSF > 0.0027322 &  DailyRateSF < 0.0054796 ~ 2,
                                               DailyRateSF < 0.0027321 ~ 1 ))

#SFSeverity dataset ----
#Creating seizure frequency severity dataset with the original output and some calculated fields and the final severity score

SFSeverity <- select(SF, -PrevRecCUI, -PrevRecDate, DATEREC_Year, -NumS, -NTP, -DailyRate, -DailyRate2,-DailyRateF, -PrevCUIFreq, -PrevRateCUI_Same6m, ) 

write_excel_csv(SFSeverity, file = "SFSeverity.csv") 
           
View(SFSeverity)

SFSeverityCUI <- select(SFSeverity, SYSTEM_ID, DATEREC, LETTER, CUI, FreqSeverity)

SFSeverityCUI <- filter(SFSeverityCUI, FreqSeverity != "NA")
View(SFSeverityCUI) # 429

#additional working outs for number of seizure free cases before the scores are added

#adding seizure groupings
SeizureType <- short_grouped_seizure_list %>% 
  select(CUI, Grouping) %>% 
  filter(!is.na(Grouping))
View(SeizureType)

SFSeverityCUI_GR <- inner_join(SFSeverityCUI, SeizureType, by ="CUI")


SFSeverityCUI_GR <- SFSeverityCUI_GR %>% 
  rename(Seizures = Grouping) 

#removing duplicates 405

 SFSeverityCUI_GR  <- SFSeverityCUI_GR %>% 
   distinct()

View(SFSeverityCUI_GR)


# these still have duplicates
seizurefree <- SFSeverity %>% 
  filter(!is.na(SeizureFree))
View(seizurefree) # gives 107 - but some just for a day

ActiveEpi <- SFSeverity %>% 
  filter(DailyRateSF > 0 & !is.na(DailyRateSF)) # gives 314
 
View(ActiveEpi) 

library(ggplot2) 
library(hrbrthemes)
library(viridis)
SFSeverityCUI_GR <- SFSeverityCUI_GR %>% arrange(Seizures, SYSTEM_ID, LETTER, DATEREC, FreqSeverity)

#selecting one person and their scores over time

One_Person <- SFSeverityCUI_GR %>% 
  filter(SYSTEM_ID == x) %>% 
  distinct()
View(One_Person)


#Plot x
One_Person %>% 
  ggplot( aes(x=DATEREC, y=FreqSeverity, group=Seizures, color=Seizures)) +
  geom_point(size = 3)+geom_line(linetype = "dashed", size = 2) + scale_colour_manual(values = c("Generalised tonic clonic seizure" = "black", "Seizures" ="grey",'Seizure free' = "gold3"))+
  scale_y_continuous(breaks= c(1,2,3,4,5,6,7), limits = c(0, 7))+labs(title="Person One - Seizure severity score", x="Record date",
  y="Seizure Severity Score")+theme (axis.text = element_text(size = 20), axis.title = element_text(size = 15), plot.title = element_text(size = 20),
  legend.text = element_text(size = 15),legend.title = element_text(size = 20))


# creating data frame with drugs

"DailyPrescriptionMaxPREF.csv"
Prescriptions <- read_delim("DailyPrescriptionMaxPREF.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)

View(Prescriptions)

#so instead of normalising we can convert the dose into a proportion of a maintenance dose (standard top dose from BNF)

Drug_Top_Doses <- read_excel("Drug_Top_Doses.xlsx")

View(Drug_Top_Doses)

PrescMDose <- left_join(Prescriptions, Drug_Top_Doses,  by = c("PREF" = "ASM"))
  

View(PrescMDose)

PrescMD <- PrescMDose %>% 
  mutate(DailyRate = as.numeric(DailyDose*100) / Dose/100)

PrescMD$DailyRate <- format(round(PrescMD$DailyRate, 2), nsmall = 2)
PrescMD$DailyRate <- as.numeric(PrescMD$DailyRate)

write_excel_csv(PrescMD, file = "PrescMD.csv") 

View(PrescMD)



SFSeverityCUI_GRNORM <- SFSeverityCUI_GR %>% 
mutate(NV = (FreqSeverity - min(FreqSeverity)) / (max(FreqSeverity) - min(FreqSeverity)))
View(SFSeverityCUI_GRNORM)

PersonOneSF <- SFSeverityCUI_GRNORM%>% 
  filter(SYSTEM_ID == x) %>% 
  distinct()
View(PersonOneSF)

#Person One seozure frequency with normalised score
PersonOneSFC <- PersonOneSF %>% 
  rename(Category = Seizures)

#person One prescription as a top dose rate  use PrescMD

PersonOneP <- PrescMD %>% 
  filter(SYSTEM_ID == x) %>% 
distinct()
  
View(PersonOneP)

# selecting only the fields needed and renaming Daily Rate as NV (normalised value) so it can be plotted with SF
PersonOnePC <- PersonOneP %>% 
  rename(Category = PREF) %>% 
select(SYSTEM_ID, LETTER, DATEREC, CUI, Category, DailyRate) %>% 
  rename(NV = DailyRate)
View(PersonOnePC)

PersonOneSFPC <- full_join(PersonOnePC , PersonOneSFC)
View(PersonOneSFPC)

#x

PersonOneSFPC %>% 
  ggplot( aes(x=DATEREC, y=NV, group=Category, color=Category, linetype = Category)) +
  geom_point(size = 4)+geom_line(size=1.2)+scale_linetype_manual("Category", values = c("Generalised tonic clonic seizure"="dashed","Seizures" ="dashed",'Seizure free' = "dashed","Levetiracetam" = "solid","Lamotrigine" = "solid" ))+
  scale_colour_manual(values = c("Generalised tonic clonic seizure" = "black","Seizures" ="grey",'Seizure free' = "gold2", "Levetiracetam" = "seagreen", "Lamotrigine" = "brown3" ))+
  labs(title="Person One - ASM and Seizure severity score ", x="Record date", y="Normalised seizure severity score and daily ASM dose as a proportion of top recommended dose")+ 
  theme (axis.text = element_text(size = 20), axis.title = element_text(size = 15), plot.title = element_text(size = 20),
         legend.text = element_text(size = 15),legend.title = element_text(size = 20))
































