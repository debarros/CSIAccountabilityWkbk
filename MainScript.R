# MainScript.R
# This script must be run before any other script is run

gc()

#-----------------------------------------#
#### Load libraries and file locations ####
#-----------------------------------------#
source("functions.R")




#-------------------------------#
#### Accountability Workbook ####
#-------------------------------#
# Get the most up to date stuff from the actual workbook
# Note: you must have access to the data drive on the school network
# This gets used in a bunch of places, including Students.R, Regents.R, Credits.R, HS Data Submission file.R,
# EnrollmentTrends.R, NewStudents.R, MeritsAndDemerits_2.R, ClassOfAndGradYear.R, etc.
Workbookraw.xlsx = read.xlsx(xlsxFile = AcctWkBkLocation, sheet = "Data", startRow = 2)
Workbook = Workbookraw.xlsx[!is.na(Workbookraw.xlsx$`Local.ID.(optional)`), ]
# Format the date variables
dateVars = c("Date.First.Enrolled.at.GTH",
             "Date.left.GTH",
             "Date.1st.Enrolled.in.9th.Grade.(anywhere)",
             "Date.of.Confirmation",
             "DOB")
for (i in dateVars) {
  Workbook[, i] = xlDate(Workbook[, i])
}




#---------------------------------#
#### PowerSchool Student Table ####
#---------------------------------#
# Read in student data from powerschool
# Note: update the data in PowerSchoolAll.xlsx before loading it
# There is a tab in the file that shows what fields to export
# This gets used in a bunch of places, such as Students.R, Regents.R, MathCourseAssignments.R, CheckEnrollments.R,
# MathRegents.R, LunchStatus.R, NewStudents.R, etc
# This is also required for loading the PowerSchool F2 grades
powerschoolraw = read.xlsx(xlsxFile = PSLocation, sheet = "Student Table")
# Format the date variables
dateVars = c("DistrictEntryDate", "EntryDate", "ExitDate")
for (i in dateVars) {
  dates = powerschoolraw[, i]
  dates[dates == "0/0/0"] = NA
  dates = as.numeric(dates)
  dates = xlDate(dates)
  powerschoolraw[, i] = dates
}




#-----------------------------#
#### PowerSchool F2 grades ####
#-----------------------------#
# Export all F2 grades from the storedgrades table in PowerSchool
# Note: The export will take a really long time
# Note: Loading this requires loading the PowerSchool students table first
# This gets used by Credits.R, FromBio.R, MathCourseAssignments.R, MathRegents.R, HS Data Submission file.R
F2 = read.xlsx(xlsxFile = PSLocation, sheet = "F2 Grades")
F2 = F2[!is.na(F2$`[1]Student_Number`),]
F2 = F2[F2$`[1]Student_Number` != 0, ]
F2$DateStored = xlDate(F2$DateStored)
F2$StudentName = powerschoolraw$lastfirst[match(F2$`[1]Student_Number`, table = powerschoolraw$student_number)]




#---------------------------------------#
#### PowerSchool Current Year grades ####
#---------------------------------------#
# Export all grades from the storedgrades table in PowerSchool, setting TermID >= current term id
# Export should also include fields [1]Student_Number and [1]LastFirst
currentGrades = read.xlsx(xlsxFile = PSLocation, sheet = "Current Year Grades")
currentGrades$DateStored = xlDate(currentGrades$DateStored)





#-----------------------------------#
#### PowerSchool Unstored Grades ####
#-----------------------------------#
# PSCB Custom Reports > Grading > Class - Percent Grades Range
# Get all records for the current term
# Click Copy Data button and paste into PowerSchool excel workbook
# This gets used in SummerSchool.R, among others
unstoredGrades = read.xlsx(xlsxFile = PSLocation, sheet = "Unstored Grades")
unstoredGrades$Last.Grade.Update = xlDate(unstoredGrades$Last.Grade.Update)





#---------------------------------#
#### Course-Subject Alignments ####
#---------------------------------#
# Sign in to google
# This gets used by MathRegents.R, MathCourseAssignments.R, CheckEnrollments.R, masterSchedule.R, etc
# Get the course-subject alignments
CourseSubject = SWSM(as_sheets_id(CourseSubjectAddress, verbose = T))                          # identify the sheet
alignment = read_sheet(ss = CourseSubject, sheet = 1)
alignment2 = read_sheet(ss = CourseSubject, sheet = 2)
alignment$Course[alignment$Course == "AP Global History I"][1] = "AP Global History I "
alignment$Course[alignment$Course == "6th Grade Attendance"][1] = "6th Grade Attendance "
FullAlignment = rbind.data.frame(alignment, alignment2, stringsAsFactors = F)






#--------------------------------#
#### Regents Scores and Stuff ####
#--------------------------------#
# This gets used in Regents.R, MathRegents.R, and maybe other places as well
Regents.scores = read.xlsx(xlsxFile = RegentsLocation, sheet = "Scores")
Regents.exams = read.xlsx(xlsxFile = RegentsLocation, sheet = "Exams")
Regents.examCategories = read.xlsx(xlsxFile = RegentsLocation, sheet = "ExamCategories")
Regents.locations = read.xlsx(xlsxFile = RegentsLocation, sheet = "Locations")
Regents.sessions =read.xlsx(xlsxFile = RegentsLocation, sheet = "Sessions")








#----------------------------#
#### PowerSchool CC Table ####
#----------------------------#
# Load the current year's enrollment records (exported from the cc table in powerschool)
# This gets used by Level0.R, CheckEnrollments.R, MathRegents.R, ClassOfAndGradYear.R, etc
cc.raw = read.xlsx(xlsxFile = PSLocation, sheet = "cc")
dateVars = c("DateEnrolled", "DateLeft")
for (i in dateVars) {
  cc.raw[, i] = xlDate(cc.raw[, i])
}
# Load the PRIOR year's enrollment records
# This gets used by ClassOfAndGradYear.R
cc_prior.raw = read.xlsx(xlsxFile = PSLocation, sheet = "cc_prior")
dateVars = c("DateEnrolled", "DateLeft")
for (i in dateVars) {
  cc_prior.raw[, i] = xlDate(cc_prior.raw[, i])
}


#----------------------#
#### SIRS Templates ####
#----------------------#
# Load all the templates (data dictionaries) for the SIRS exports
# This gets used by a bunch of scripts, including LunchStatus.R, Level0.R, HS Data Submission file.R, etc.
templates = loadWorkbook(TemplateLocation)





#------------------------#
#### SATs, PSATs, etc ####
#------------------------#
# This gets used by PSATScores.R, Level0.R, HS Data Submission file.R

# Load SAT data
SAT.raw = read.xlsx(
  xlsxFile = CollegeBoardLocation,
  sheet = "SAT",
  startRow = 1,
  na.strings = c("")
)

# Load PSAT data
PSAT.raw = read.xlsx(xlsxFile = CollegeBoardLocation,
                     sheet = "PSAT",
                     na.strings = c(""))

# Load CLEP data
CLEP.raw = read.xlsx(xlsxFile = CollegeBoardLocation,
                     sheet = "CLEP",
                     na.strings = c(""))
CLEP.raw$Test.Date = xlDate(CLEP.raw$Test.Date)

# Load AP data
AP.raw = read.xlsx(xlsxFile = CollegeBoardLocation,
                   sheet = "AP",
                   na.strings = c(""))
AP.raw$Test.Date = xlDate(AP.raw$Test.Date)

# Load ACT data
# ACT.raw = read.xlsx(xlsxFile = CollegeBoardLocation, sheet = "ACT")

# Load college courses
college.raw = read.xlsx(xlsxFile = CollegeBoardLocation, sheet = "College Courses")


#-----------------#
#### DOR Table ####
#-----------------#
# Load the table of Districts of Residence.  These are typed up, not exported from anywhere.
# This gets used in Students.R and LunchStatus.R
DORs = read.xlsx(xlsxFile = PSLocation, sheet = "DOR")





#------------------------#
#### Test Name Lookup ####
#------------------------#
# Load the table of exam names.  These are typed up, not exported from anywhere.
# This gets used in Regents.R
testLookup = read.xlsx(xlsxFile = PSLocation, sheet = "Test Names")



#------------------------#
#### Attendance Codes ####
#------------------------#
# Load the table of attendance codes.
# This is an export of the entire Attendance_Code table in PowerSchool.
attendCodes = read.xlsx(xlsxFile = PSLocation, sheet = "Attendance Codes")



#-------------------------------#
#### Period Codes and Lookup ####
#-------------------------------#
# Load the table of period codes.
# This is an export of the entire Period table in PowerSchool.
periodCodes = read.xlsx(xlsxFile = PSLocation, sheet = "Periods")
periodLookup = read.xlsx(xlsxFile = PSLocation, sheet = "PeriodLookup")



#------------------#
#### Attendance ####
#------------------#
# Load the table of attendance entries.
# This is an export of the Attendance table in PowerSchool, with YearID set to the current year.
attendance = read.xlsx(xlsxFile = PSLocation, sheet = "Attendance")
dateVars = c("Att_Date")
for (i in dateVars) {
  attendance[, i] = xlDate(attendance[, i])
}




#------------------------#
#### Current Students ####
#------------------------#
# This gets used in MathCourseAssignments.R, MathRegents.R, and possibly other places
currentStudents = drive_download(
  as_id(CurrentStudentsAddress),
  path = paste0(OutFolder, "currentStudents.csv"),
  overwrite = T
)
currentStudents = read.csv(paste0(OutFolder, "currentStudents.csv"), stringsAsFactors = F)






#------------------------------------#
#### PowerSchool Current Sections ####
#------------------------------------#
# This gets used in CheckEnrollments.R, LoginMailings.R, etc
sections = read.xlsx(xlsxFile = PSLocation, sheet = "Sections")



