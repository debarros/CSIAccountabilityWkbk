#MainScript.R

#---------------------------#
#### Load and clean data ####
#---------------------------#

#This section must be run before any other script is run

source("functions.R")

#Get the most up to date stuff from the actual workbook
#Note: you must have access to the data drive on the school network
Workbookraw.xlsx = read.xlsx(
  xlsxFile = "J:/Accountability Spreadsheet/working copy/Green Tech Cohort Data Collection Workbook.xlsx", 
  sheet = "All info in 1 sheet",startRow = 2)
Workbook = Workbookraw.xlsx[!is.na(Workbookraw.xlsx$`Local.ID.(optional)`),-c(1,2,3)]

# Remove those who never attended
# Note: don't do this if you are updating regents scores or other things in the workbook
Workbook = Workbook[!(VbetterComp(Workbook$Discharge.Reason, "never attended")),]
rownames(Workbook) = NULL

#Format the date variables
dateVars = c("Date.First.Enrolled.at.GTH", "Date.left.GTH", "Date.1st.Enrolled.in.9th.Grade.(anywhere)","Date.of.Confirmation")
for(i in dateVars){ Workbook[,i] = as.Date(Workbook[,i], origin = "1899-12-30") }



# Below are variables that are important to get from powerschool
# Note that the code currently assumes subsets of this data for the powerschool regents and powerschool students files
# 
# student_number
# lastfirst
# grade_level
# RCT_Reading_Score
# RCT_Global_History_&_Geography_Score
# Regents_Spanish_Score
# Regents_Global_History_&_Geography_Score
# Regents_Earth_Science_Score
# Regents_French_Score
# RCT_US_History_&_Government
# RCT_Science_Score
# RCT_Math_Score
# RCT_Writing_Score
# Regents_US_History_Score
# Regents_Algebra_Score
# Regents_Chemistry_Score
# Regents_Geometry_Score
# Regents_Living_Environment_Score
# Regents_Algebra2/Trigonometry_Score
# Regents_Comprehensive_English_Score
# Regents_Physics_Score
# Last_Name
# First_name
# DistrictEntryDate
# Enroll_Status
# EntryDate
# Ethnicity
# ExitCode
# ExitDate

