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



# Below are variables that are important to get from the students table in powerschool
# Note that the code currently assumes subsets of this data for the powerschool regents and powerschool students files
# However, they should be rewritten to use the more general PowerSchoolAll.xlsx, which includes all the variables below.
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
powerschoolraw = read.xlsx(xlsxFile = "PowerSchoolAll.xlsx", sheet = 1)


# Export all F2 grades from the storedgrades table in PowerSchool
F2 = read.xlsx(xlsxFile = "F2grades.xlsx")
F2$DateStored = as.Date(F2$DateStored, origin = "1899-12-30")
F2$StudentName = powerschool$lastfirst[match(F2$`[1]Student_Number`, table = powerschool$student_number)]


# Sign in to google
gs_auth() #this will launch a browser so you can sign into your account

# Get the course-subject alignments
CourseSubject = gs_url("https://docs.google.com/a/greentechhigh.org/spreadsheets/d/17QhVYZkjbx34M6wBvtHUYa_XrRUlRbOtuOsQ4P5l-nk/edit?usp=sharing")
alignment = gs_read(ss = CourseSubject, ws = 1)
alignment2 = gs_read(ss = CourseSubject, ws = 2)
alignment$Course[alignment$Course == "AP Global History I"][1] = "AP Global History I "
FullAlignment = rbind.data.frame(alignment, alignment2, stringsAsFactors = F)



#Get the the Output query from the Regents database (located at data drive/database project/regents.accdb)
#Paste it into the files RegentsDB.csv in the folder for this project
#note: if new exams (not just new scores) have been entered, 
#      the sql code for the Output query will need to be modified 
#      to pull that exam's scores and dates from TestScoreOutput and TestDateOutput
RegentsDBraw = read.csv("RegentsDB.csv", stringsAsFactors = FALSE) 
