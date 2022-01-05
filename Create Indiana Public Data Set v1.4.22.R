################################
# Create Indiana Public File Data Set
# Last Modified: 1.5.2022
################################

#Goal is to set up the initial panel data set with all publicly-available measures

##### Set up packages #####

library(tidyverse)
library(openxlsx)
library(data.table)
library(tictoc)
options(scipen = 999)

setwd("C:/Users/egrebing/Box/CELL Indiana EIR - Team/Impact Analysis/Indiana Public Administrative Data")

### Original Matching Data Set from the Survey Analysis ###
#Note - This data set includes only schools meeting the rural criteria and has coding for the treatment and comparison schoolss
survey.matching.data <- fread("RECN Full Matching Data Set 2.4.21.csv", stringsAsFactors = F) %>%
  select(ST_SCHID:TREAT) %>%
  mutate(Name_Mod = gsub("Jr-Sr", "Jr Sr", NAME),
         Name_Mod = gsub("-|/", " ", Name_Mod)) %>%
  filter(!grepl("South Central|Eastern High|Washington High|North Central|Southwestern High", NAME)) %>% #Remove schools because of ambiguity
  filter(!grepl("Bloomfield|Charlestown|Connersville|Hobart|Lawrenceburg|Mississinewa|New Washington|Richmond|Tell City", NAME)) %>% #Remove endorsed programs
  
  #Add cohort coding
  mutate(Cohort = case_when(
    grepl("5075-4911|3945-3239|0875-0701|6375-6654|6825-7125", ST_SCHID) ~ "Tier 1",
    grepl("2475-2083|3115-2565|2275-1733|6155-6581|6080-6513", ST_SCHID) ~ "Tier 2",
    grepl("8525-9137|8115-8737|3055-2463|5520-5985|2110-1588", ST_SCHID) ~ "Tier 3",
    TRUE ~ "Comp Pool"
  )) %>%
  arrange(desc(TREAT), Cohort, NAME)
  
### Enrollment ###

#Enrollment (Year is the spring of the school year; 2021 <-> 2020-21)
enrl.21 <- read.xlsx("school-enrollment-grade-2006-21.xlsx", sheet = "2021", sep.names = "_") %>%
  mutate(Year = 2021)

enrl.20 <- read.xlsx("school-enrollment-grade-2006-21.xlsx", sheet = "2020", sep.names = "_") %>%
  mutate(Year = 2020)

enrl.19 <- read.xlsx("school-enrollment-grade-2006-21.xlsx", sheet = "2019", sep.names = "_") %>%
  mutate(Year = 2019)

enrl.18 <- read.xlsx("school-enrollment-grade-2006-21.xlsx", sheet = "2018", sep.names = "_") %>%
  mutate(Year = 2018)

enrl.17 <- read.xlsx("school-enrollment-grade-2006-21.xlsx", sheet = "2017", sep.names = "_") %>%
  mutate(Year = 2017) %>%
  select(Corp_ID, Corp_Name = Corporation_Name, Schl_ID, Schl_Name = School_Name, 
         `Pre-K`:`Grade_12+/Adult`, TOTAL_ENROLLMENT = Grand_Total, Year)

enrl.16 <- read.xlsx("school-enrollment-grade-2006-21.xlsx", sheet = "2016", sep.names = "_") %>%
  mutate(Year = 2016) %>%
  select(Corp_ID, Corp_Name = Corporation_Name, Schl_ID, Schl_Name = School_Name, 
         `Pre-K`:`Grade_12+/Adult`, TOTAL_ENROLLMENT = Grand_Total, Year)

enrl.15 <- read.xlsx("school-enrollment-grade-2006-21.xlsx", sheet = "2015", sep.names = "_") %>%
  mutate(Year = 2015) %>%
  select(Corp_ID, Corp_Name = Corporation_Name, Schl_ID, Schl_Name = School_Name, 
         `Pre-K`:`Grade_12+/Adult`, TOTAL_ENROLLMENT = Grand_Total, Year)

table(names(enrl.15)==names(enrl.21))

enrl <- bind_rows(enrl.15, enrl.16, enrl.17, enrl.18, enrl.19, enrl.20, enrl.21) %>%
  mutate(ST_SCHID = paste0("IN-", Corp_ID, "-", Schl_ID)) %>%
  select(ST_SCHID, Corp_Name, Schl_Name, Year, TOTAL_ENROLLMENT, Grade_6:Grade_12) %>%
  filter(ST_SCHID %in% survey.matching.data$ST_SCHID) %>%
  pivot_longer(cols = Grade_6:Grade_12) %>%
  mutate(name = paste0(name, "_N"),
         value = ifelse(is.na(value), 0, value)) %>%
  pivot_wider(names_from = name, values_from = value, values_fill = list(value = 0)) %>%
  rowwise() %>%
  mutate(Grade_9_12_Enrl = Grade_9_N + Grade_10_N + Grade_11_N + Grade_12_N,
         Grade_6_8_Enrl = Grade_6_N + Grade_7_N + Grade_8_N) %>%
  ungroup()

rm(list = ls(pattern = "enrl."))

###################

### Mode of Instruction and Attendance (2020-21) ###
mode.inst.attnd <- read.xlsx("2020-2021-Mode-of-Instruction-and-Attendance-percentage-20210709.xlsx", 
                             sheet = "Data", sep.names = "_") %>%
  mutate(ST_SCHID = paste0("IN-", `Corp_Id,_if_applicable`, "-", School_Id)) %>%
  select(ST_SCHID,
         Virtual_Wks_Pct_2021 = `Percent_of_weeks_during_the_2020-2021_School_year_the_school_provided_Virtual_Instruction_(as_Primary_Mode)`,
         In_Person_Wks_Pct_2021 = `Percent_of_weeks_during_the_2020-2021_School_year_the_school_provided_In-Person_Instruction_(as_Primary_Mode)`,
         Hybrid_Wks_Pct_2021 = `Percent_of_weeks_during_the_2020-2021_School_year_the_school_provided_Hybrid_Instruction_(as_Primary_Mode)`,
         Attendance_Rate_2021 = `2020-2021_Attendance_Rate`) %>%
  mutate(Attendance_Rate_2021 = as.numeric(Attendance_Rate_2021)) %>%
  filter(ST_SCHID %in% survey.matching.data$ST_SCHID)

################

### College Readiness Data (By Graduating Cohort) ###
#Note: These data sets do not have any IDs

coll.read.19 <- read.xlsx("2021-College-Readiness-Dataset-2019-Cohort.xlsx", sheet = "Data", sep.names = "_")
coll.read.18 <- read.xlsx("2020-College-Readiness-Dataset-2018-Cohort.xlsx", sheet = "Data", sep.names = "_") %>%
  select(Cohort, LocationType = GeoType, Location:Breakout, HSGradsN = HSGradN, everything())

coll.read.17 <- read.xlsx("2019-College-Readiness-Dataset-2017-Cohort.xlsx", sheet = "Data", sep.names = "_") %>%
  select(Cohort, LocationType = GeoType, Location:Breakout, HSGradsN = HSGradN, everything())

table(names(coll.read.19)==names(coll.read.17))

coll.read.17.19 <- bind_rows(coll.read.17, coll.read.18, coll.read.19) %>%
  select(-GroupSort, -DisaggSort)

rm(coll.read.17, coll.read.18, coll.read.19) 

#The 2008 to 2016 data have additional columns associated with persistence
coll.read.08.16 <- read.xlsx("College-Readiness-Dataset-2008-2016-a.xlsx", sheet = "DATA", sep.names = "_") %>%
  select(names(coll.read.17.19))

coll.read <- bind_rows(coll.read.17.19, coll.read.08.16) %>%
  select(Grad_Cohort = Cohort, everything())

coll.read.school <- coll.read %>%
  filter(LocationType == "School") %>%
  mutate(Year = Grad_Cohort) %>%
  filter(Year >= 2015)

#Create a lookup table for the school names to get accurate matches with ST_SCHID
school.names.coll.read.auto <- coll.read.school %>%
  mutate(Location_Mod = gsub("Jr-Sr", "Jr Sr", Location),
         Location_Mod = gsub("/", " ", Location_Mod)) %>%
  select(Location_Mod, Location) %>%
  distinct() %>%
  separate(col = Location_Mod, into = c("School", "Corp1"), sep = "-", remove = F) %>%
  left_join(survey.matching.data, by = c("School" = "Name_Mod"))

#Manually change names to match between the two data sets
school.names.coll.read.manual <- school.names.coll.read.auto %>%
  filter(is.na(ST_SCHID)) %>%
  select(-(ST_SCHID:TREAT), -Cohort) %>%
  mutate(Location_Mod = gsub("Bedford-North Lawrence", "Bedford North Lawrence", Location_Mod),
         Location_Mod = gsub("Franklin County High School", "Franklin County High", Location_Mod),
         Location_Mod = gsub("Shoals Comm", "Shoals Community", Location_Mod),
         Location_Mod = gsub("Blue River Valley Jr Sr High School", "Blue River Valley Jr Sr High Sch", Location_Mod),
         Location_Mod = gsub("Clinton Central Junior-Senior High School", "Clinton Central Junior Senior HS", Location_Mod),
         Location_Mod = gsub("Crawford County Jr Sr High School", "Crawford County High School", Location_Mod),
         Location_Mod = gsub("Culver Community High School", "Culver Community Middle High Sch", Location_Mod),
         Location_Mod = gsub("Eastside Junior-Senior High School", "Eastside Junior Senior High School", Location_Mod),
         Location_Mod = gsub("Jac-Cen-Del", "Jac Cen Del", Location_Mod),
         Location_Mod = gsub("Lanesville Jr Sr High School", "Lanesville Jr Sr HS", Location_Mod),
         Location_Mod = gsub("Linton-Stockton", "Linton Stockton", Location_Mod),
         Location_Mod = gsub("Judson-San Pierre", "Judson San Pierre", Location_Mod),
         Location_Mod = gsub("Oregon-Davis", "Oregon Davis", Location_Mod),
         Location_Mod = gsub("Rossville Middle Senior High School", "Rossville Middle Senior High Sch", Location_Mod),
         Location_Mod = gsub("South Knox Middle-High School", "South Knox Middle High School", Location_Mod),
         Location_Mod = gsub("Tri-County Middle-Senior High", "Tri County Middle Senior High", Location_Mod),
         Location_Mod = gsub("Tri-West", "Tri West", Location_Mod),
         Location_Mod = gsub("Tri Central Middle-High School", "Tri Central Middle High School", Location_Mod),
         Location_Mod = gsub("Tri Junior-Senior High School", "Tri Junior Senior High School", Location_Mod),
         Location_Mod = gsub("Union City Community Jr Sr HS", "Union City Community Jr Sr High", Location_Mod),
         Location_Mod = gsub("Waldo J Wood Memorial High School", "Waldo J Wood Memorial High", Location_Mod),
         Location_Mod = gsub("Wes-Del", "Wes Del", Location_Mod),
         Location_Mod = gsub("White River Valley Jr Sr High School", "White River Valley High School", Location_Mod)) %>%
  separate(col = Location_Mod, into = c("School", "Corp1"), sep = "-", remove = F) %>%
  left_join(survey.matching.data, by = c("School" = "Name_Mod")) %>%
  arrange(desc(TREAT), Location_Mod)

#Combine automatic and manual matches
school.names.coll.read <- school.names.coll.read.auto %>%
  bind_rows(school.names.coll.read.manual) %>%
  filter(!is.na(ST_SCHID)) %>%
  distinct()

#Add IDs to the coll.read.school data
coll.read.school.sample <- coll.read.school %>%
  inner_join(school.names.coll.read, by = "Location") %>%
  arrange(desc(TREAT), NAME, Grad_Cohort)

#Count schools by year - Note Parke Heritage first appears in the 2019 data
coll.read.school.sample %>%
  select(Location, Grad_Cohort, ST_SCHID, TREAT) %>%
  distinct() %>%
  count(TREAT, Grad_Cohort)

#Get the averages only for the analysis
coll.read.school.sample.averages <- coll.read.school.sample %>%
  filter(Category == "Average") %>%
  arrange(desc(TREAT), Location_Mod, Grad_Cohort) %>%
  select(Year, HS_Grad_Cohort = Grad_Cohort, Location, ST_SCHID, NAME, TREAT, HSGradsN:AvgFreshmanCreditEarned) %>%
  mutate_at(vars(HSGradsN:AvgFreshmanCreditEarned), ~as.numeric(.))

coll.read.school.sample.den <- coll.read.school.sample %>%
  filter(Category == "Average") %>%
  arrange(desc(TREAT), Location_Mod, Grad_Cohort) %>%
  select(ST_SCHID, Year, HS_Grad_Cohort = Grad_Cohort, HSGradsN) %>%
  mutate(HSGradsN = as.numeric(HSGradsN))

coll.read.school.dual.credit <- coll.read.school.sample %>%
  filter(grepl("Earned Dual Credit", Breakout)) %>%
  select(ST_SCHID, Year, Dual_Credit_Count = HSGradsN) %>%
  mutate(Earned_Dual_Credit_N = as.numeric(Dual_Credit_Count)) %>%
  left_join(coll.read.school.sample.den, by = c("ST_SCHID", "Year")) %>%
  select(ST_SCHID, Year, HS_Grad_Cohort, HSGradsN, everything()) %>%
  mutate(Dual_Credit_Earned_Pct = Earned_Dual_Credit_N / HSGradsN)

coll.read.school.sat <- coll.read.school.sample %>%
  filter(grepl("SAT", Category)) %>%
  mutate(Breakout = gsub(" |-", "_", Breakout),
         Breakout = gsub("<", "LT", Breakout),
         Breakout = gsub("SAT_", "", Breakout),
         Breakout = paste0("SAT_", Breakout)) %>%
  select(ST_SCHID, Year, Breakout, count = HSGradsN) %>%
  mutate(count = as.numeric(count),
         Breakout = paste0(Breakout, "_N")) %>%
  pivot_wider(names_from = Breakout, values_from = count) %>%
  left_join(coll.read.school.sample.den, by = c("ST_SCHID", "Year")) %>%
  select(ST_SCHID, Year, HS_Grad_Cohort, HSGradsN, everything()) %>%
  mutate(Took_SAT_N = HSGradsN - SAT_No_data_N,
         Took_SAT_Pct = Took_SAT_N / HSGradsN)

coll.read.school.act <- coll.read.school.sample %>%
  filter(grepl("ACT", Category)) %>%
  mutate(Breakout = gsub(" |-", "_", Breakout),
         Breakout = gsub("<", "LT", Breakout),
         Breakout = gsub("ACT_", "", Breakout),
         Breakout = paste0("ACT_", Breakout)) %>%
  select(ST_SCHID, Year, Breakout, count = HSGradsN) %>%
  mutate(count = as.numeric(count),
         Breakout = paste0(Breakout, "_N")) %>%
  pivot_wider(names_from = Breakout, values_from = count) %>%
  left_join(coll.read.school.sample.den, by = c("ST_SCHID", "Year")) %>%
  select(ST_SCHID, Year, HS_Grad_Cohort, HSGradsN, everything()) %>%
  mutate(Took_ACT_N = HSGradsN - ACT_No_data_N,
         Took_ACT_Pct = Took_ACT_N / HSGradsN)

coll.read.school.benchmark <- coll.read.school.sample %>%
  filter(grepl("Benchmark", Category)) %>%
  mutate(Breakout = case_when(
    grepl("Did Not Meet", Breakout) ~ "Benchmk_Not_Met",
    grepl("Did Not Take", Breakout) ~ "Benchmk_Not_Taken",
    grepl("Met ACT", Breakout) ~ "Benchmk_Met")) %>%
  select(ST_SCHID, Year, Breakout, count = HSGradsN) %>%
  mutate(count = as.numeric(count)) %>%
  pivot_wider(names_from = Breakout, values_from = count) %>%
  left_join(coll.read.school.sample.den, by = c("ST_SCHID", "Year")) %>%
  select(ST_SCHID, Year, HS_Grad_Cohort, HSGradsN, everything()) %>%
  rowwise() %>%
  mutate(Met_Coll_Ready_Benchmk_Pct = Benchmk_Met / HSGradsN,
         Not_Taken_Coll_Ready_Benchmk_Pct = Benchmk_Not_Taken / HSGradsN) %>%
  select(-starts_with("Benchmk")) %>%
  ungroup()

coll.read.school.ap <- coll.read.school.sample %>%
  filter(grepl("Advanced Placement", Category)) %>%
  mutate(Breakout = case_when(
    grepl("Did Not Take", Breakout) ~ "AP_Not_Taken",
    grepl("Did Not Pass", Breakout) ~ "AP_Not_Passed",
    grepl("Took and Passed", Breakout) ~ "AP_Passed")) %>%
  select(ST_SCHID, Year, Breakout, count = HSGradsN) %>%
  mutate(count = as.numeric(count)) %>%
  pivot_wider(names_from = Breakout, values_from = count) %>%
  left_join(coll.read.school.sample.den, by = c("ST_SCHID", "Year")) %>%
  select(ST_SCHID, Year, HS_Grad_Cohort, HSGradsN, everything()) %>%
  rowwise() %>%
  mutate(Passed_AP_Pct = AP_Passed / HSGradsN,
         Took_AP_Pct = (AP_Passed + AP_Not_Passed) / HSGradsN) %>%
  select(-starts_with("AP")) %>%
  ungroup()

coll.read.school.21st.scholar <- coll.read.school.sample %>%
  filter(grepl("^21st", Breakout)) %>%
  select(ST_SCHID, Year, Breakout, count = HSGradsN) %>%
  mutate(count = as.numeric(count)) %>%
  left_join(coll.read.school.sample.den, by = c("ST_SCHID", "Year")) %>%
  select(ST_SCHID, Year, HS_Grad_Cohort, HSGradsN, Scholar_21st_Century_N = count) %>%
  mutate(Scholar_21st_Cen_Pct = Scholar_21st_Century_N / HSGradsN)

coll.read.school.avg <- coll.read.school.sample.averages %>%
  pivot_longer(cols = EnrCollegeN:AvgFreshmanCreditEarned,
               names_to = "measure") %>%
  mutate(Pct = ifelse(grepl("N$", measure), 
                      value / HSGradsN,
                      value)) %>%
  mutate(Measure_Mod = case_when(
    measure == "EnrCollegeN" ~ "Pct_Enr_College",
    measure == "EnrINPubCollegeN" ~ "Pct_Enr_IN_Pub_College",
    measure == "NeedRemedN" ~ "Pct_Need_Remed",
    measure == "EarnRemedCredN" ~ "Pct_Earn_Remed_Cred",
    measure == "AvgFreshmanGPA" ~ "Avg_Freshman_GPA",
    measure == "AvgFreshmanCreditEarned" ~ "Avg_Freshman_Credit_Earned"
  )) %>%
  select(-measure, -value) %>%
  pivot_wider(names_from = Measure_Mod, values_from = Pct) %>%
  select(ST_SCHID, Year, HS_Grad_Cohort, HSGradsN, everything())

college.readiness.out <- coll.read.school.avg %>%
  left_join(coll.read.school.dual.credit, by = c("ST_SCHID", "Year", "HS_Grad_Cohort", "HSGradsN")) %>%
  left_join(coll.read.school.ap, by = c("ST_SCHID", "Year", "HS_Grad_Cohort", "HSGradsN")) %>%
  left_join(coll.read.school.benchmark, by = c("ST_SCHID", "Year", "HS_Grad_Cohort", "HSGradsN")) %>%
  left_join(coll.read.school.act, by = c("ST_SCHID", "Year", "HS_Grad_Cohort", "HSGradsN")) %>%
  left_join(coll.read.school.sat, by = c("ST_SCHID", "Year", "HS_Grad_Cohort", "HSGradsN")) %>%
  left_join(coll.read.school.21st.scholar, by = c("ST_SCHID", "Year", "HS_Grad_Cohort", "HSGradsN")) %>%
  select(-Location, -NAME, -TREAT, -HS_Grad_Cohort) %>%
  select(ST_SCHID, Year, HS_Grads_N = HSGradsN, everything())

rm(list = ls(pattern = "coll.read|school.names"))

####################

### Demographics ###
race.frpl.21 <- read.xlsx("school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-21.xlsx",
                          sheet = "2021", sep.names = "_") %>%
  mutate(Year = 2021)

race.frpl.20 <- read.xlsx("school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-21.xlsx",
                          sheet = "2020", sep.names = "_") %>%
  mutate(Year = 2020)

race.frpl.19 <- read.xlsx("school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-21.xlsx",
                          sheet = "2019", sep.names = "_") %>%
  mutate(Year = 2019)

race.frpl.18 <- read.xlsx("school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-21.xlsx",
                          sheet = "2018", sep.names = "_") %>%
  mutate(Year = 2018)

race.frpl.17 <- read.xlsx("school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-21.xlsx",
                          sheet = "2017", sep.names = "_") %>%
  mutate(Year = 2017)

race.frpl.16 <- read.xlsx("school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-21.xlsx",
                          sheet = "2016", sep.names = "_") %>%
  mutate(Year = 2016)

race.frpl.15 <- read.xlsx("school-enrollment-ethnicity-and-free-reduced-price-meal-status-2006-21.xlsx",
                          sheet = "2016", sep.names = "_") %>%
  mutate(Year = 2015)

table(names(race.frpl.21)==names(race.frpl.18))

race.frpl.18.21 <- bind_rows(race.frpl.18, race.frpl.19, race.frpl.20, race.frpl.21)
race.frpl.15.17 <- bind_rows(race.frpl.15, race.frpl.16, race.frpl.17)

names(race.frpl.15.17) <- names(race.frpl.18.21)

race.frpl <- bind_rows(race.frpl.15.17, race.frpl.18.21) %>%
  mutate(ST_SCHID = paste0("IN-", Corp_ID, "-", Schl_ID)) %>%
  select(ST_SCHID, Year, American_Indian:TOTAL_ENROLLMENT) %>%
  pivot_longer(cols = American_Indian:Paid_Meals) %>%
  mutate(name = gsub("/", "_", name),
         name = paste0(name, "_N"),
         value = ifelse(is.na(value), 0, value)) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  filter(ST_SCHID %in% survey.matching.data$ST_SCHID) %>%
  mutate(Pct_White = White_N / TOTAL_ENROLLMENT,
         Pct_FRPL = Free_Reduced_Price_Meals_N / TOTAL_ENROLLMENT) %>%
  select(-TOTAL_ENROLLMENT)

rm(list = ls(pattern="race.frpl."))

########################################

### Special Education and ELL ###
sped.ell.21 <- read.xlsx("school-enrollment-ell-special-education-2006-21.xlsx", 
                         sheet = "2021", sep.names = "_") %>%
  mutate(Year = 2021)

sped.ell.20 <- read.xlsx("school-enrollment-ell-special-education-2006-21.xlsx", 
                         sheet = "2020", sep.names = "_") %>%
  mutate(Year = 2020)

sped.ell.19 <- read.xlsx("school-enrollment-ell-special-education-2006-21.xlsx", 
                         sheet = "2019", sep.names = "_") %>%
  mutate(Year = 2019)

sped.ell.18 <- read.xlsx("school-enrollment-ell-special-education-2006-21.xlsx", 
                         sheet = "2018", sep.names = "_") %>%
  mutate(Year = 2018)

sped.ell.17 <- read.xlsx("school-enrollment-ell-special-education-2006-21.xlsx", 
                         sheet = "2017", sep.names = "_") %>%
  mutate(Year = 2017)

sped.ell.16 <- read.xlsx("school-enrollment-ell-special-education-2006-21.xlsx", 
                         sheet = "2016", sep.names = "_") %>%
  mutate(Year = 2016)

sped.ell.15 <- read.xlsx("school-enrollment-ell-special-education-2006-21.xlsx", 
                         sheet = "2015", sep.names = "_") %>%
  mutate(Year = 2015)

sped.ell <- bind_rows(sped.ell.15, sped.ell.16, sped.ell.17, sped.ell.18,
                      sped.ell.19, sped.ell.20, sped.ell.21) %>%
  mutate(ST_SCHID = paste0("IN-", Corp_ID, "-", Schl_ID)) %>%
  filter(ST_SCHID %in% survey.matching.data$ST_SCHID) %>%
  arrange(School_Name, Year) %>%
  select(ST_SCHID, Year, starts_with("ELL"), starts_with("Special"))

names(sped.ell) <- gsub("%", "Pct", names(sped.ell))

rm(list = ls(pattern = "sped.ell."))

################################

### Attendance ###
attendance <- read.xlsx("schoolattendancerates.xlsx", sheet = "Attendance Rate", sep.names = "_", startRow = 2) %>%
  mutate(ST_SCHID = paste0("IN-", Corporation_Id, "-", School_Id)) %>%
  pivot_longer(cols = `2019-20`:`2005-06`, names_to = "Year", values_to = "Attendance_Rate") %>%
  mutate(Year = as.numeric(paste0(substr(Year, 1, 2), substr(Year, 6, 7)))) %>%
  filter(Year >= 2015) %>%
  filter(ST_SCHID %in% survey.matching.data$ST_SCHID) %>%
  arrange(ST_SCHID, Year) %>%
  select(ST_SCHID, Year, Attendance_Rate)

##################################

### Graduation Rate ###
grad.rate.21 <- read.xlsx("2021-Indiana-State-Graduation-Rate.xlsx", sheet = "School Public NonWaiver",
                          sep.names = "_", startRow = 2) %>%
  mutate(Year = 2021,
         ST_SCHID = paste0("IN-", Corp_Id, "-", School_Id)) %>%
  mutate_at(vars(Cohort_N:`2021_State_Non-Waiver_Grad_Rate`), as.numeric) %>%
  select(ST_SCHID, Year, Cohort_N, Grad_N, NoWaiver_Grad_N = `Non-Waiver_Grad_N`,
         Grad_Rate_Total = `2021_State_Grad_Rate`, Grad_Rate_NoWaiver = `2021_State_Non-Waiver_Grad_Rate`)

#Note: For 2020, the 'School Public Nonwaiver' tab was not released.
grad.rate.20 <- read.xlsx("2020-state-grad-rate-data-20210115.xlsx", sheet = "School Pub Disagg",
                          sep.names = "_", startRow = 3, check.names = T) %>%
  mutate(Year = 2020,
         ST_SCHID = paste0("IN-", Corp_Id, "-", School_Id)) %>%
  select(ST_SCHID, Year, Cohort_Count = Cohort_Count.13, Graduates = Graduates.13, Grad_Rate = Graduation_Rate.13) %>%
  mutate_at(vars(Cohort_Count:Grad_Rate), as.numeric) %>%
  select(ST_SCHID, Year, Cohort_N = Cohort_Count, Grad_N = Graduates, Grad_Rate_Total = Grad_Rate)

grad.rate.19 <- read.xlsx("2019-state-grad-rate-data-20191231.xlsx", sheet = "School Public NonWaiver",
                          sep.names = "_", startRow = 2) %>%
  mutate(Year = 2019,
         ST_SCHID = paste0("IN-", Corp_Id, "-", School_Id)) %>%
  mutate_at(vars(Cohort_N:`2019_State_Non-Waiver_Grad_Rate`), as.numeric) %>%
  select(ST_SCHID, Year, Cohort_N, Grad_N, NoWaiver_Grad_N = `Non-Waiver_Grad_N`,
         Grad_Rate_Total = `2019_State_Grad_Rate`, Grad_Rate_NoWaiver = `2019_State_Non-Waiver_Grad_Rate`)

grad.rate.18 <- read.xlsx("2018-state-grad-rate-data20190204.xlsx", sheet = "School Public NonWaiver",
                          sep.names = "_", startRow = 2) %>%
  mutate(Year = 2018,
         ST_SCHID = paste0("IN-", Corp_Id, "-", School_Id)) %>%
  mutate_at(vars(Cohort_N:`2018_State_Non-Waiver_Grad_Rate`), as.numeric) %>%
  select(ST_SCHID, Year, Cohort_N, Grad_N, NoWaiver_Grad_N = `Non-Waiver_Grad_N`,
         Grad_Rate_Total = `2018_State_Grad_Rate`, Grad_Rate_NoWaiver = `2018_State_Non-Waiver_Grad_Rate`)

grad.rate.17 <- read.xlsx("2017-graduation-rate-04-17-2018-publication.xlsx", sheet = "School_Public_NonWaiver",
                          sep.names = "_", startRow = 1) %>%
  mutate(Year = 2017,
         ST_SCHID = paste0("IN-", Corp_ID, "-", Schl_ID)) %>%
  mutate_at(vars(Cohort_N:`Non-Waiver_Grad_Rate`), as.numeric) %>%
  select(ST_SCHID, Year, Cohort_N, Grad_N, NoWaiver_Grad_N = `Non-Waiver_Grad_N`,
         Grad_Rate_Total = Grad_Rate, Grad_Rate_NoWaiver = `Non-Waiver_Grad_Rate`)

grad.rate.16 <- read.xlsx("2016-graduation-rate-02-09-2017.xlsx", sheet = "School_Public_NonWaiver",
                          sep.names = "_", startRow = 1) %>%
  mutate(Year = 2016,
         ST_SCHID = paste0("IN-", Corp_ID, "-", Sch_ID)) %>%
  mutate_at(vars(Cohort_N:`Non-Waiver_Grad_Rate`), as.numeric) %>%
  select(ST_SCHID, Year, Cohort_N, Grad_N, NoWaiver_Grad_N = `Non-Waiver_Grad_N`,
         Grad_Rate_Total = Grad_Rate, Grad_Rate_NoWaiver = `Non-Waiver_Grad_Rate`)

grad.rate.15 <- read.xlsx("2015graduationrate.xlsx", sheet = "School_Public_NonWaiver",
                          sep.names = "_", startRow = 1) %>%
  mutate(Year = 2015,
         ST_SCHID = paste0("IN-", Corp_ID, "-", Sch_ID)) %>%
  mutate_at(vars(Cohort_N:`Non-Waiver_Grad_Rate`), as.numeric) %>%
  select(ST_SCHID, Year, Cohort_N, Grad_N, NoWaiver_Grad_N = `Non-Waiver_Grad_N`,
         Grad_Rate_Total = Grad_Rate, Grad_Rate_NoWaiver = `Non-Waiver_Grad_Rate`)

grad.rate <- bind_rows(grad.rate.15, grad.rate.16, grad.rate.17, grad.rate.18,
                       grad.rate.19, grad.rate.20, grad.rate.21) %>%
  arrange(ST_SCHID, Year) %>%
  filter(ST_SCHID %in% survey.matching.data$ST_SCHID)

rm(list = ls(pattern = "grad.rate."))

############################

### Combine into the full data set ###
recn.qed.matching.data <- enrl %>%
  left_join(race.frpl, by = c("ST_SCHID", "Year")) %>%
  left_join(sped.ell, by = c("ST_SCHID", "Year")) %>%
  left_join(attendance, by = c("ST_SCHID", "Year")) %>%
  left_join(grad.rate, by = c("ST_SCHID", "Year")) %>%
  left_join(college.readiness.out, by = c("ST_SCHID", "Year"))