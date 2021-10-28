#Step by step analysis of seizure frequency output from ExECT. Based on the output from separate documents
#using DOC as reference. DateRec can be used instead to identify individual records, although for some letters this can be missing.

library(readr)
library(sqldf)
library(RSQLite)
library(proto)
library(gsubfn)
library(dplyr)
library(tibble)
library(zoo)
library(lubridate)

options(scipen = 999) #numbers should not be displayed in scientific notation
options(digits = 6) #Number of digits to be displayed


# 1 seizures per time period ----

SF <- read_delim("SF_output.csv", delim = ":", escape_double = FALSE, na = "null", trim_ws = TRUE)

SF$DATEREC <-as.Date(SF$DATEREC, "%d/%m/%Y") 
View(SF)
head(SF)
#Creating a single column "NumS" from number of seizures (NofS) and Upper number of seizures (UNofS) 
#ignoring lower no of seizures as no values without the upper range. 

SF <- SF %>%  mutate(NumS = case_when(
  !is.na(NofS) ~ NofS,
  is.na(NofS) & UNofS>0 ~ UNofS
)) 
SF$NumS <- as.numeric(SF$NumS)
#2 Number of time periods ----
# Creating a single column "NumTP" from number of time periods (NofTP) and  lower number of time periods (LNofTP) 
#ignoring upper number of time periods as we want the most frequent seizures from the final calculation

SF <- SF %>% mutate(NumTP = case_when(
  NofTP>0 & !is.na(NofTP) ~ NofTP,
  is.na(NofTP) & LNofTP>0 ~ LNofTP
)
)
SF$NumTP <- as.numeric(SF$NumTP)
#3 Number of Seizures per Time Period ----
#adding a column SperTP which is the result of calculating the number of seizures per stated time period

SF <- SF %>% mutate(SperTP = NumS/NumTP, .after = "NumTP")

#4 Seizure frequency per day ----

#creates a column with all time periods as days, we are trying to compare seizure frequency per different time periods, 
#days would give a common denominator but it could be done in weeks or months.
SF <- SF %>% mutate(Days = case_when(TP == "Day" ~ 1,
                                     TP == "Week" ~ 7,
                                     TP == "Month" ~ 30,
                                     TP == "Year" ~ 365))


#creates a column giving number of seizures per time period in days

SF <- SF %>% mutate(DailyRate = SperTP/Days) #we take a previously calculated number of seizures per time period and convert it to days


#5 EventDate based on separate DD,MD, and YD columns ----

#converting numerical values given as days (DD) and months (MD) to two figire format
SF <- SF %>% mutate(DD = ifelse(!is.na(DD) & as.numeric(DD) < 10,paste("0", DD, sep = "") ,DD)) # convert from 9 to 09 for date

SF <- SF %>% mutate(MD = ifelse(!is.na(MD) & as.numeric(MD) < 10,paste("0", MD, sep = "") ,MD))


SF <- SF %>% mutate(EventDate = ifelse(!is.na(YD) & !is.na(MD) & !is.na(DD), paste(DD, MD, YD, sep = "/"),#All day month year there
                                       ifelse(!is.na(YD) & !is.na(MD) & is.na(DD), paste("01", MD, YD, sep = "/"),## no day only month and year
                                              ifelse(!is.na(YD) & is.na(MD) & is.na(DD), paste("01","01", YD, sep = "/"),# year only
                                                     ifelse(is.na(YD) & !is.na(MD) & is.na(DD), paste("01", MD, ifelse(as.numeric(MD) <  as.numeric(format(DATEREC, "%m")), format(DATEREC, "%Y"), as.character(as.numeric(format(DATEREC, "%Y"))-1)), sep = "/"), #month only - using DATEREC but it could be clinic or letter date, get year of or year before (if month mentioned is after month of DATEREC)
                                                            ifelse(is.na(YD) & !is.na(MD) & !is.na(DD), paste(DD, MD, ifelse(as.numeric(MD) <  as.numeric(format(DATEREC, "%m")), format(DATEREC, "%Y"), as.character(as.numeric(format(DATEREC, "%Y")) -1)), sep = "/"), NA))))), .after = "DATEREC")#As above but with day also (in above set the 1st of month)

#we could add here EventDate2 for cases when there are no date values and event was in Point in time (PinT) "Last_Month" or "Last_Week" using DATEREC and some date calculations
SF <- SF %>% mutate(EventDate2  = case_when(is.na(YD) & is.na(MD) & is.na(DD) & PinT == "Last_Month" ~ DATEREC - 30, #when Point in time is last month we can take 30 days away from DATEREC
                                            is.na(YD) & is.na(MD) & is.na(DD) & PinT == "Last_Year" ~ DATEREC - 365), .after = "EventDate") #when Point in time is last year we can take 365 days away from DATEREC

SF$EventDate <- as.Date(SF$EventDate, "%d/%m/%Y")   
SF$EventDate2 <- as.Date(SF$EventDate2, "%d/%m/%Y")  

#6 Previous seizure frequency for specific CUI ----

SF <- SF %>% arrange(SF, SYSTEM_ID, DATEREC, CUI) # ordering data by SYSTEM_ID and date of record first - really important as we are using functions that look back in time

#PrevRecCUI finds a date of previous clinic - going back up to 6 rows in records sorted by DATEREC /CUI  for the particular CUI i.e. seizure type
SF <- SF %>%  mutate(PrevRecCUI = as.Date( ifelse(CUI == lag(CUI) & SYSTEM_ID == lag(SYSTEM_ID) & DATEREC > lag(DATEREC), lag(DATEREC),
                                                  ifelse(CUI == lag(CUI, 2) & SYSTEM_ID == lag(SYSTEM_ID, 2) & DATEREC > lag(DATEREC, 2), lag(DATEREC, 2),
                                                         ifelse(CUI == lag(CUI, 3) & SYSTEM_ID == lag(SYSTEM_ID, 3) & DATEREC > lag(DATEREC, 3) ,lag(DATEREC, 3),     
                                                                ifelse(CUI == lag(CUI, 4) & SYSTEM_ID == lag(SYSTEM_ID, 4) & DATEREC > lag(DATEREC, 4), lag(DATEREC, 4), 
                                                                       ifelse(CUI == lag(CUI, 5) & SYSTEM_ID == lag(SYSTEM_ID, 5)& DATEREC > lag(DATEREC, 5), lag(DATEREC, 5),
                                                                              ifelse(CUI == lag(CUI, 6) & SYSTEM_ID == lag(SYSTEM_ID, 6) & DATEREC > lag(DATEREC, 6), lag(DATEREC, 6), NA))))))),.after = "DATEREC" )




#7 Difference in days ----
#difference in days between DATEREC and event date to calculate number of seizures per day for that period

SF <- SF %>% mutate(DaysDiff=case_when(!is.na(EventDate) ~ DATEREC - EventDate, #this one uses event date created from dates 
                                       is.na(EventDate) & PinT == "LastClinic" & !is.na(PrevRecCUI) ~ DATEREC - PrevRecCUI, # this one uses previous clinic date for the particular CUI 
                                       is.na(EventDate) & PinT == "Last_Month" |PinT == "Last_Year" ~ DATEREC - EventDate2)) # this one uses event date based on point in time
SF$DaysDiff <- as.numeric(SF$DaysDiff)

#8 Seizures since per day ----
#shows seizures per day reported as seizures since  (using dates) as DailyRate2

SF <- SF %>% mutate(DailyRate2 = NumS/DaysDiff )

SF <- SF %>% mutate(DailyRateF = case_when(!is.na(DailyRate) ~ DailyRate,
                                           is.na(DailyRate) ~ DailyRate2),.after = "DailyRate2")


#9 Seizure free ----
#Shows number of days seizure free as based on 0 seizures for a number of days (calculated field) or 0 seizures in the number of days calculated from
#event day or last clinic

SF <- SF %>% mutate(SeizureFree = case_when(NumS == 0 & !is.na(Days) ~ NumTP*Days, #as the seizure per time period (SperTP) willalways be 0 for 0 seizures we need to take the original number of TP and multiply by days
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


#10 "Same" in FreqChange ----
#if seizure frequency is reported as "same"  in FreqChange find a letter that is the most recent to the date of "Same" record and use PrevCUIFreq for that record

SF <- SF %>% mutate(PrevRateCUI_Same6m = case_when(FreqChange == "Same" & DATEREC - PrevRecCUI > 182 ~ PrevCUIFreq))

#Adding previous < 6m CUI specific seizure frequency to daily rate 

SF <- SF %>% mutate(DailyRateSF = case_when(!is.na(DailyRateF) ~ DailyRateF,
                                            is.na(DailyRateF) ~ PrevRateCUI_Same6m))


#11 Adding frequency scores ----
#Frequency score calculated as per day rate with seizure free given priority 
#using the calculated columns of daily rate (DailyRateSF final rate and rate for "Same" frequency under FreqChange) 
#Frequency scores used here are based on  Fitzgerald MP,  et al. Assessing seizure burden in pediatric epilepsy using an electronic ...

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

SFSeverity <- select(SF, -PrevRecCUI, -NumS, -NumTP, -SperTP, -DailyRate, -DailyRate2,-DailyRateF, -PrevCUIFreq, -PrevRateCUI_Same6m) 

write_excel_csv(SFSeverity, file = "SFSeverity") 

View(SFSeverity)

SFSeverityCUI <- select(SFSeverity, SYSTEM_ID, DATEREC, CUI, FreqSeverity)

SFSeverityCUI <- filter(SFSeverityCUI, FreqSeverity != "NA")

library(ggplot2) 
library(hrbrthemes)
library(viridis)
SFSeverityCUI <- SFSeverityCUI %>% arrange(SFSeverityCUI, SYSTEM_ID, DATEREC, FreqSeverity)


#Some experiments 

#Creating a subset of 4 IDs to illustrate seizure frequency
#Ignoring CIU as looking at the maximum frequency reported

SFSeverityAll <- select(SFSeverityCUI, SYSTEM_ID, DATEREC, FreqSeverity)

ID <- c('a' ,'b','c', 'd')
FrequencyScore <- SeverityAll %>% 
  group_by(DATEREC) %>% 
  filter(FreqSeverity == max(FreqSeverity)) %>% 
  arrange(SYSTEM_ID, DATEREC, FreqSeverity)

View(FrequencyScore)

FrequencyScore$SYSTEM_ID <- as.factor(FrequencyScore$SYSTEM_ID)

#Plot
FrequencyScore %>% 
  ggplot( aes(x=DATEREC, y=FreqSeverity, group=SYSTEM_ID, color=SYSTEM_ID)) +
  geom_line() + geom_point() 
  


View(FrequencyScore)
Score <- Severity$FreqSeverity
DATEREC <- Severity$DATEREC
Data <- data.frame(DATEREC,Score)
ggplot(Data, aes(x = DATEREC, y = Score,)) + geom_point(aes(color = factor(SYSTEM_ID)))


