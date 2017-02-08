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
Workbook = Workbook[!(VbetterComp(Workbook$Discharge.Reason, "never attended")),]

#Format the date variables
dateVars = c("Date.First.Enrolled.at.GTH", "Date.left.GTH", "Date.1st.Enrolled.in.9th.Grade.(anywhere)","Date.of.Confirmation")
for(i in dateVars){ Workbook[,i] = as.Date(Workbook[,i], origin = "1899-12-30") }

