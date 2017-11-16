# MainScript.R

#---------------------------#
#### Load and clean data ####
#---------------------------#

# This section must be run before any other script is run

source("functions.R")

# Get the most up to date stuff from the actual workbook
# Note: you must have access to the data drive on the school network
Workbookraw.xlsx = read.xlsx(
  xlsxFile = "\\\\stuthin2\\Data\\Accountability Spreadsheet\\working copy\\Green Tech Cohort Data Collection Workbook.xlsx",
  sheet = "All info in 1 sheet",startRow = 2)
Workbook = Workbookraw.xlsx[!is.na(Workbookraw.xlsx$`Local.ID.(optional)`),-c(1,2,3)]


#Format the date variables
dateVars = c("Date.First.Enrolled.at.GTH", "Date.left.GTH", "Date.1st.Enrolled.in.9th.Grade.(anywhere)","Date.of.Confirmation")
for(i in dateVars){  Workbook[,i] = as.Date(Workbook[,i], origin = "1899-12-30") }

# Read in student data from powerschool
# Note: update the data in PowerSchoolAll.xlsx before loading it
# There is a tab in the file that shows what fields to export
powerschoolraw = read.xlsx(xlsxFile = "PowerSchoolAll.xlsx", sheet = 1)


# Export all F2 grades from the storedgrades table in PowerSchool
# Note: The export will take a really long time
F2 = read.xlsx(xlsxFile = "PowerSchoolAll.xlsx", sheet = "F2 Grades")
F2 = F2[F2$`[1]Student_Number` != 0,]
F2$DateStored = as.Date(F2$DateStored, origin = "1899-12-30")
F2$StudentName = powerschoolraw$lastfirst[match(F2$`[1]Student_Number`, table = powerschoolraw$student_number)]


# Sign in to google
gs_auth() #this may launch a browser so you can sign into your account

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




# Load the current term's enrollment records (exported from the cc table in powerschool)
cc.raw = read.xlsx(xlsxFile = "PowerSchoolAll.xlsx", sheet = "cc")


# Load all the templates (data dictionaries) for the SIRS exports
wblocation = "\\\\stuthin2/Data/SIRS manuals templates guides etc/2017-18eScholarTemplatesNYS_2017-09-25.xlsx"
templates = loadWorkbook(wblocation)






