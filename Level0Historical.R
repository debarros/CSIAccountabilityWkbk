#Level0Historical.R

# load functions and data ####
source("functions.R")

#Get the most up to date stuff from the actual workbook
Workbookraw.xlsx = read.xlsx(
  xlsxFile = "J:/Accountability Spreadsheet/working copy/Green Tech Cohort Data Collection Workbook.xlsx", 
  sheet = "All info in 1 sheet",startRow = 2)
Workbook = Workbookraw.xlsx[!is.na(Workbookraw.xlsx$`Local.ID.(optional)`),-c(1,2,3)]

# Clean the data ####

# Remove those who never attended and check the remaining discharge reasons
x = Workbook$Discharge.Reason != "never attended"
x[is.na(x)] = T
Workbook = Workbook[x,]

#Clean the entry and exit date variables
Workbook$Date.First.Enrolled.at.GTH = as.Date(Workbook$Date.First.Enrolled.at.GTH, origin = "1899-12-30")
Workbook$Date.left.GTH = as.Date(Workbook$Date.left.GTH, origin = "1899-12-30")


# Fix historical data ####

#Subset to the relevant students - entered last year, but with a different cohort year
cohort = "2015"
idStart = paste0(substr(cohort,3,4), 1+as.integer(substr(cohort,3,4)))

StudentsOfInterest = Workbook[substr(as.character(Workbook$`Local.ID.(optional)`),1,4) == idStart & Workbook$`Cohort.Year.(year.1st.entered.9th)` != as.integer(cohort),]

View(StudentsOfInterest[,c(2:4,13)])

#Go through each student.  Look him up in L0H.  
#Check his data of entry into grade 9 against the first record actually showing an enrollment in 9th grade.
#Check the assessment records against the regents database.