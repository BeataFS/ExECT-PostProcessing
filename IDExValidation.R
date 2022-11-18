#Validating IDEx extracted Epi25 information against SNB extract for 
#Epi25 SAIL  Genetics set

library(grid)
library(ggplot2)
library(xlsx)
library(readr)

A = data.frame(C = c(DoB$DATE_OF_BIRTH))
B = data.frame (C = c(SAIL_Link_File1$DATE_OF_BIRTH))

A$C %in% B$C

A$C[A$C %in% B$C]

ggvenn(a, c("DoB_IDEx","DoB_SNB"), fill_colour = c("red","green"), stroke_size = 0.5, set_name_size = 5)
  
DoB

install.packages("sqldf")
library(sqldf)
library(RSQLite)
library(proto)
library(gsubfn)
library(xlsx)

# DoB ----
# per letter analysis without grouping, using SAIL_LINK_FILE 1 which holds the SNB data

DoB_per_let <- sqldf("SELECT a.SYSTEM_ID, a.DATE_OF_BIRTH, b.DATE_OF_BIRTH
            FROM DoB a
            LEFT JOIN SAIL_Link_File1 b ON  
            a.DATE_OF_BIRTH=b.DATE_OF_BIRTH")

View(DoB_per_let)
# number of DoB annotations per individual set
DoB_count <- sqldf("SELECT SYSTEM_ID, COUNT (SYSTEM_ID) as DoB_count FROM DoB GROUP BY SYSTEM_ID") 
View(DoB_count)
write_excel_csv(DoB_count, file = "DoB_count")
# ID_count shows the numbers of letters that have a date of birth, but now we need to check the values

LJ <- sqldf("SELECT a.SYSTEM_ID, a.DATE_OF_BIRTH
            FROM DoB a
            LEFT JOIN SAIL_Link_File1 b ON
            a.DATE_OF_BIRTH=b.DATE_OF_BIRTH
            GROUP BY a.SYSTEM_ID")
View(LJ)

# Identifying unmatched cases 
no_match <- sqldf("SELECT SYSTEM_ID FROM SAIL_Link_File1
                  WHERE SYSTEM_ID NOT IN (SELECT SYSTEM_ID FROM DoB)")

View(no_match) #table is empty, no false positives 

#NHS ----
NHS <- read_csv("NHS.csv") # NHS No output from IDEx
View(NHS)

NHS_count <- sqldf("SELECT SYSTEM_ID, COUNT (SYSTEM_ID) as NHS_count FROM NHS GROUP BY SYSTEM_ID")
View(NHS_count)
write_excel_csv(NHS_count, file = "NHS_count")
NHS_match <- sqldf("SELECT a.SYSTEM_ID, a.NHS_NUMBER
            FROM NHS a
            LEFT JOIN SAIL_Link_File1 b ON
            a.NHS_NUMBER=b.NHS_NUMBER
            GROUP BY a.SYSTEM_ID")
View(NHS_match)

# Identifying false negatives for NHS No annotations
no_match <- sqldf("SELECT SYSTEM_ID FROM SAIL_Link_File1 
WHERE SYSTEM_ID NOT IN (SELECT SYSTEM_ID FROM NHS)")
                  
View(no_match) # 2 cases

#Gender ----
Gender <- read_csv("Gender.csv") # Gender output from IDEx
View(Gender)
Gender_count <- sqldf("SELECT SYSTEM_ID, COUNT (SYSTEM_ID) 
                   as Gender_count FROM Gender GROUP BY SYSTEM_ID")
View(Gender_count)
write_excel_csv(Gender_count, file = "Gender_count")
Gender_match <- sqldf("SELECT a.SYSTEM_ID, a.GENDER_CD
            FROM Gender a
            LEFT JOIN SAIL_Link_File1 b ON
            a.GENDER_CD=b.GENDER_CD
            GROUP BY a.SYSTEM_ID")
View(Gender_match) # all 111 match
no_match <- sqldf("SELECT SYSTEM_ID FROM SAIL_Link_File1 
WHERE SYSTEM_ID NOT IN (SELECT SYSTEM_ID FROM Gender)")
View(no_match) 

#PostCode ----
PostCode <- read_csv("PostCode.csv") # Postcode output from IDEx
PostCode_count <- sqldf("SELECT SYSTEM_ID, COUNT (SYSTEM_ID) 
                   as PostCode_count FROM PostCode GROUP BY SYSTEM_ID")
write_excel_csv(PostCode_count, file = "PostCode_count")

PostCode_match <- sqldf("SELECT a.SYSTEM_ID, a.POSTCODE
            FROM PostCode a
            LEFT JOIN SAIL_Link_File1 b ON
            a.POSTCODE=b.PostCode
            GROUP BY a.SYSTEM_ID")
View(PostCode_match) # all 111 match
no_match <- sqldf("SELECT SYSTEM_ID FROM SAIL_Link_File1 
WHERE SYSTEM_ID NOT IN (SELECT SYSTEM_ID FROM Gender)")
View(no_match) 

#Linking the ClinicDate and LeterDate  IDEx outputs to create single table ----
setwd <- ("C:/Users/Beata/Documents/Epi25letters/IDExOutput") #set working directory

Epi25_ClinicDate <- read_delim("Epi25_ClinicDate.csv", 
                               delim = ":", escape_double = FALSE, trim_ws = TRUE)
Epi25_LetterDate <- read_delim("Epi25_LetterDate.csv", 
                               delim = ":", escape_double = FALSE, trim_ws = TRUE)

View(Epi25_ClinicDate)

#Full join is not available in sqldf so need to load dplyr

library(dplyr)

Clinic_Letter_Date <- full_join(Epi25_ClinicDate, Epi25_LetterDate, by="LETTER")

View(Clinic_Letter_Date)

Clin_Let_Date <- select(Clinic_Letter_Date, "LETTER", "Clinic_Date", "LetDate")


View(Clin_Let_Date)
#Comparing List of Epi25 clinic letters to the list of clinic and letter dates

 Letterlist <- read_delim("Epi25_LetterList.csv", 
                                    delim = ":", escape_double = FALSE, trim_ws = TRUE)
 
 
 View(Letterlist)
 
 NoDate <- anti_join(Letterlist,Clin_Let_Date, by="LETTER")
 
 UniqueLet <- distinct(Clin_Let_Date, LETTER)
 
 View(UniqueLet)
 View(NoDate)

 
 write_excel_csv(Clin_Let_Date, file = "Epi25_Clin_Let_Date.csv")
 
 
 Clin_Let_Date_DOC <- full_join(Clin_Let_Date, Letterlist, by="LETTER")
 
 View(Clin_Let_Date_DOC)  #one letters gets duplicated for no apparent r reason
 
 Clin_Let_Date_DOC %>% 
   group_by(DOC) %>% 
   filter(n()>1)
 
 
 View(Clin_Let_Date_DOC)
 
 
 
 
 Epi25_RecDate_DOC$Clinic_Date <- as.Date(Epi25_RecDate_DOC$Clinic_Date, "%d/%m/%Y")
 
 noClinicDate <- select(Epi25_RecDate_DOC, "Clinic_Date" = NA)
 
 
library(data.table)
?getDTthreads
setwd <- ("C:/Users/Beata/Documents/Epi25letters/IDExOutput")
files <- list.files(pattern = ".csv")

RStudio.Version()


library(caret)

#Validating Clinic date, letter date, and record date (Date_Rec) on a sample of 200 randomly selected letters

#changing working dir to ExECTOutput as this is where the files are but will transfer the results oy IdEX validation
setwd <- ("C:/Users/Beata/Documents/Epi25letters/ExECTOutput")

RecDate <- read_delim("Epi25_RecDate_DOC.csv", 
                               delim = ":", escape_double = FALSE, trim_ws = TRUE)

X200_random <- read_csv("C:/Users/Beata/Documents/Epi25letters/IDEx_Validation/200_random.csv")

View(X200_random)

X200_random_dates <- left_join(X200_random, RecDate, by="LETTER")

View(X200_random_dates)

write_excel_csv(X200_random_dates, file = "X200_random_dates.csv")
           
X200_random_dates$Clinic_Date <- as.Date(X200_random_dates$Clinic_Date, "%d/%m/%Y")

X200_random_dates$LetDate <- as.Date(X200_random_dates$LetDate, "%d/%m/%Y")

dplyr::filter(X200_random_dates, Clinic_Date > LetDate)

library(dplyr)

  
 
