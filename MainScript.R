# MainScript.R
# This script must be run before any other script is run



#-----------------------------------------#
#### Load libraries and file locations ####
#-----------------------------------------#
source("functions.R")



#-------------------------------#
#### Accountability Workbook ####
#-------------------------------#
# Get the most up to date stuff from the actual workbook
# Note: you must have access to the data drive on the school network
Workbookraw.xlsx = read.xlsx(
  xlsxFile = AcctWkBkLocation,
  sheet = "All info in 1 sheet",startRow = 2)
Workbook = Workbookraw.xlsx[!is.na(Workbookraw.xlsx$`Local.ID.(optional)`),-c(1,2,3)]
#Format the date variables
dateVars = c("Date.First.Enrolled.at.GTH", "Date.left.GTH", "Date.1st.Enrolled.in.9th.Grade.(anywhere)","Date.of.Confirmation")
for(i in dateVars){  Workbook[,i] = as.Date(Workbook[,i], origin = "1899-12-30") }



#---------------------------------#
#### PowerSchool Student Table ####
#---------------------------------#
# Read in student data from powerschool
# Note: update the data in PowerSchoolAll.xlsx before loading it
# There is a tab in the file that shows what fields to export
powerschoolraw = read.xlsx(xlsxFile = PSLocation, sheet = 1)



#-----------------------------#   What is this for?
#### PowerSchool F2 grades ####
#-----------------------------#
# Export all F2 grades from the storedgrades table in PowerSchool
# Note: The export will take a really long time
F2 = read.xlsx(xlsxFile = PSLocation, sheet = "F2 Grades")
F2 = F2[F2$`[1]Student_Number` != 0,]
F2$DateStored = as.Date(F2$DateStored, origin = "1899-12-30")
F2$StudentName = powerschoolraw$lastfirst[match(F2$`[1]Student_Number`, table = powerschoolraw$student_number)]



#---------------------------------#
#### Course-Subject Alignments ####
#---------------------------------#
# Sign in to google
gs_auth() #this may launch a browser so you can sign into your account
# Get the course-subject alignments
CourseSubject = SWSM(gs_url(CourseSubjectAddress))
alignment = SWSM(gs_read(ss = CourseSubject, ws = 1))
alignment2 = SWSM(gs_read(ss = CourseSubject, ws = 2))
alignment$Course[alignment$Course == "AP Global History I"][1] = "AP Global History I "
FullAlignment = rbind.data.frame(alignment, alignment2, stringsAsFactors = F)



#------------------------#
#### Regents Database ####
#------------------------#
#Get the the Output query from the Regents database (located at data drive/database project/regents.accdb)
#Paste it into the files RegentsDB.csv in the folder for this project
#note: if new exams (not just new scores) have been entered, 
#      the sql code for the Output query will need to be modified 
#      to pull that exam's scores and dates from TestScoreOutput and TestDateOutput
RegentsDBraw = read.csv(RDBLocation, stringsAsFactors = FALSE) 



#----------------------------#
#### PowerSchool CC Table ####
#----------------------------#
# Load the current term's enrollment records (exported from the cc table in powerschool)
cc.raw = read.xlsx(xlsxFile = PSLocation, sheet = "cc")



#----------------------#
#### SIRS Templates ####
#----------------------#
# Load all the templates (data dictionaries) for the SIRS exports
templates = loadWorkbook(TemplateLocation)





#-----------------#
#### DOR Table ####
#-----------------#
# Load the table of Districts of Residence.  These are typed up, not exported from anywhere.
DORs = read.xlsx(xlsxFile = PSLocation, sheet = "DOR")





#------------------------#
#### Test Name Lookup ####
#------------------------#
# Load the table of exam names.  These are typed up, not exported from anywhere.
testLookup = read.xlsx(xlsxFile = PSLocation, sheet = "Test Names")



#------------------------#
#### SATs, PSATs, etc ####
#------------------------#
# Load SAT data
SAT.raw = read.xlsx(xlsxFile = CollegeBoardLocation, sheet = "SAT")

# Load PSAT data
PSAT.raw = read.xlsx(xlsxFile = CollegeBoardLocation, sheet = "PSAT", na.strings = c(""))

# Load AP data
AP.raw = read.xlsx(xlsxFile = CollegeBoardLocation, sheet = "AP")

# Load ACT data
ACT.raw = read.xlsx(xlsxFile = CollegeBoardLocation, sheet = "ACT")



