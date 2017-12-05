# LunchStatus.R

# To run this, you need the following:
#   A recent download of the state matches from the NYSSIS website
#   A recent Program Facts extract from Power school
#   A recent Demographics extract from Power school
#   A recent Enrollment extract from Power school


#-----------------#
#### Load Data ####
#-----------------#

# get the file of state matches downloaded from the nyssis website
# You must open the file in excel, save it, and close it again before importing it here
# Make sure that it goes in the lunch folder for the current year, and begins with "nyssis_lunch"

# Load all of them from this year and then combine them into one data.frame
NyssisFile = read.xlsx.multi(folder = LunchLocation, pattern = "nyssis_lunch", dupVar = "Local.ID", orderBy = "Certification.Method")

# generate the enrollment, student lite, and program service extracts from PowerSchool and load them here
EnrollmentExtract = read.csv(file = file.choose(), header = F, stringsAsFactors = F)
StudentLiteExtract = read.csv(file = file.choose(), header = F, stringsAsFactors = F)
progserv = read.csv(file = file.choose(), header = F)

# Add column names to the SIRS extracts
colnames(progserv) = GetNiceColumnNames("PROGRAMS FACT", templates)
colnames(StudentLiteExtract) = GetNiceColumnNames("STUDENT LITE", templates)[1:ncol(StudentLiteExtract)]
colnames(EnrollmentExtract) = GetNiceColumnNames("SCHOOL ENTRY EXIT", templates)

# Get just the lunch services
lunch = progserv[progserv$PROGRAMSCODEPROGRAMSERVICECODE %in% c(5806, 5817),]
# Students who currently are set as Reduced Price lunch in powerschool
reducedLunch = lunch[lunch$PROGRAMSCODEPROGRAMSERVICECODE == 5806,]            
# Students who currently are set as Free lunch in powerschool
freeLunch = lunch[lunch$PROGRAMSCODEPROGRAMSERVICECODE == 5817,]               

# Get the Nyssis Matches
allMatches = NyssisFile[NyssisFile$Local.ID %in% StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID,
                        c("Local.ID", "P12.First.Name","P12.Last.Name", "Certification.Method","Case.Numbers")]


#-----------------------------------------------------#
#### Bulk Upload 1 - determining students to check ####
#-----------------------------------------------------#
# Only run this section if you plan to make a bulk upload to submit to the NYSSIS site
# This should only be done once per year

# Which of this year's students do not appear in the NYSSIS extract?
StudentsToCheck = setdiff(StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID, NyssisFile$Local.ID)
write(x = paste0(StudentsToCheck, collapse = ","), file = "studentsToCheck.txt")

# Go to the current working directory, open the text file, and search powerschool for those students.  
# Export them with all relevant fields.
# Paste the data into an excel workbook


#----------------------------------------------#
#### Bulk Upload 2 - making the upload file ####
#----------------------------------------------#

# Only do this next part if you have already done Bulk Upload 1
# NOTE: THIS FUNCTION HAS NOT BEEN TESTED
CNMS.MakeBulkUpload(xlsxFile = "studentsToCheck (2017-10-05).xlsx", singleGender = "M")
# Upload the file bulkupload.txt to the NYSSIS web interface


#-------------------------------------------------#
#### Bulk Upload 3 - Using Bulk Upload Matches ####
#-------------------------------------------------#

# Only do this next part if Bulk Upload process has completed
# Wait 24 hours after running Bulk Upload 1, then go back into NYSSIS.  
# Click on the number of records next to the upload.  Download the results.
# Go through the results and find matches.  When matches are found, enter them on the Results tab.
# Once the results tab has been populated, continue:

bulkMatches = read.xlsx(xlsxFile = "\\\\stuthin2/data/2017-2018/lunch/Bulk Search Results (2017-10-10).xlsx", sheet = "Results")
allMatches = rbindlist(l = list(allMatches, bulkMatches))
allMatches = allMatches[order(allMatches$Certification.Method, decreasing = T),]
allMatches = allMatches[!duplicated(allMatches$Local.ID),]
summary(allMatches)
summary(factor(allMatches$Certification.Method))
write.csv(x = allMatches, file = "allmatches.csv")

unmatchedIDs = setdiff(StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID, allMatches$Local.ID) # ID's of unmatched students 
unmatchedIDs = StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID %in% unmatchedIDs # Logical, which rows have unmatched students
write.csv(x = StudentLiteExtract[unmatchedIDs, 4:8], file = "unmatchedStudents.csv") # output unmatched students

# Inform the relevant people about the number of matches.
# Send the two csv's to whoever needs them.
# The students in unmatchedStudents.csv need to submit lunch forms


#----------------------------------------------------#
#### PowerSchool Upload and Changes based on DCMP ####
#----------------------------------------------------#

# Get set of students for new upload
newmatches = allMatches[!(allMatches$Local.ID %in% lunch$STUDENTIDSCHOOLDISTRICTSTUDENTID),]
if(nrow(newmatches) > 0){
  print("see the newmatches.csv file")
  print("Use the program service upload template to import these into PowerSchool.")
  print("For the eligibility code, use 'DCMP'.")
  write.csv(x = newmatches, file = "newmatches.csv")
} else {
  print("There are no new matches to upload to PowerSchool")
} # /if-else


# Students who are set as reduced price lunch, but should be set as free lunch
reducedToSwitch = allMatches$Local.ID %in% reducedLunch$STUDENTIDSCHOOLDISTRICTSTUDENTID
if(sum(reducedToSwitch) > 0){
  print("The following students are set as Reduced Price lunch, but need to be changed to Free Lunch.")
  print(allMatches[reducedToSwitch,])
} else {
  print("There are no students who need to be changed from Reduced Price Lunch to Free Lunch in PowerSchool")
} # /if-else


# Students who currently have free lunch, but not the DCMP eligibility code
freeLunch$DCMP = FALSE        # Initialize the DCMP variable
for(i in 1:nrow(freeLunch)){  # Determine which students have the DCMP code
  freeLunch$DCMP[i] = any(freeLunch[i,paste0("PROGRAMELIGIBILITYCODE",1:6)] == "DCMP")
} # /for
freeAddDCMP = freeLunch[!freeLunch$DCMP,] # Subset to just those who don't have the DCMP code
freeAddDCMP = allMatches$Local.ID %in% freeAddDCMP$STUDENTIDSCHOOLDISTRICTSTUDENTID  # Find students with free lunch in powerschool who need the DCMP code
if(sum(freeAddDCMP) > 0){
  print("The following students are set as Free lunch, but need to have DCMP added to their program eligibility codes.")
  print(allMatches[freeAddDCMP,])
} else {
  print("There are no Free Lunch students in PowerSchool that need to have DCMP added to their program eligibility codes.")
} # /if-else


#-----------------------------------------------#
#### PowerSchool Upload based on Lunch Forms ####
#-----------------------------------------------#

statusTypes = data.frame(
  Status = c("F", "R", "D", "Free", "Reduced", "Denied", "Paid"), 
  Code = c("F", "R", "D", "F", "R", "D", "D"), 
  stringsAsFactors = F)

forms = read.xlsx(xlsxFile = file.choose()) # read in the Lunch Applications file from Ms Hood
forms = forms[!is.na(forms$Lunch.Form),]    # leave out the ones with no lunch form
forms = forms[!is.na(forms$Status),]        # leave out the ones with no lunch status
forms$Status = statusTypes$Code[match(forms$Status, statusTypes$Status)]
forms = forms[!is.na(forms$Status),]        # leave out the ones with no lunch status
forms = forms[forms$Status != "D",]         # leave out the ones who are Denied (paid lunch)
forms = forms[!(forms$Student.Number %in% allMatches$Local.ID),] # leave out the ones who are already in allMatches
forms = forms[!(forms$Student.Number %in% lunch$STUDENTIDSCHOOLDISTRICTSTUDENTID),] # leave out the ones who are already in PowerSchool
if(nrow(forms) > 0){
  write.csv(x = forms, file = "newFormMatches.csv")
  print("There are new matches to upload")
  print("see the file newFormMatches.csv")
  print("Use the program service upload template to import these into PowerSchool.")
  print("For the eligibility code, use 'Application'.")
} else {
  print("No new matches from the forms")
} # /if-else


#-----------------------------#
#### Breakdown by District ####
#-----------------------------#

# This section is useful for the annual Title 1 report that gets submitted through a web form

StudentLiteExtract$EnrolledOnBEDSDay = F
StudentLiteExtract$EnrolledByBEDSDay = F
EnrollmentExtract$SCHOOLEXITDATEENROLLMENTEXITDATE[EnrollmentExtract$SCHOOLEXITDATEENROLLMENTEXITDATE == ""] = schoolYear("end")
BEDSday = BedsDate()

for(i in 1:nrow(StudentLiteExtract)){
  enrollRows = EnrollmentExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID == StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID[i]
  enrollments = EnrollmentExtract[enrollRows,]
  for(j in 1:nrow(enrollments)){
    enrollments$Before[j] = enrollments$SCHOOLENTRYDATEENROLLMENTENTRYDATE[j] <= BEDSday
    enrollments$After[j]  = enrollments$SCHOOLEXITDATEENROLLMENTEXITDATE[j] >= BEDSday
    enrollments$Both[j] = enrollments$Before[j] & enrollments$After[j]
  } # /for each of that student's enrollment records
  StudentLiteExtract$EnrolledByBEDSDay[i] = any(enrollments$Before)
  StudentLiteExtract$EnrolledOnBEDSDay[i] = any(enrollments$Both)
} # /for each student
StudentLiteExtract$Lunch = factor(
  lunch$PROGRAMSCODEPROGRAMSERVICECODE[match(
    StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID, 
    lunch$STUDENTIDSCHOOLDISTRICTSTUDENTID)])
summary(StudentLiteExtract$Lunch)

# Use the following line to determine which students to use
# Subset it by EnrolledOnBEDSDay or EnrolledByBEDSDay, if desired.
# Otherwise, don't subset it at all and just use the whole StudentLiteExtract
usableStudents = StudentLiteExtract[StudentLiteExtract$EnrolledOnBEDSDay,]

LunchByDistrict = table(usableStudents[,c("Lunch", "DISTRICTCODEOFRESIDENCE")], useNA = "always")
DistrictCodes = dimnames(LunchByDistrict)$DISTRICTCODEOFRESIDENCE
DORlookup = as.data.frame(matrix(c(
  "NY010100", 	"Albany",
  "NY270100", 	"Amsterdam",
  "NY491302", 	"Averill Park",
  "NY010306", 	"Bethlehem",
  "NY541102", 	"Coblskl-Rchmdvl",
  "NY010500", 	"Cohoes",
  "NY490301", 	"East Greenbush",
  "NY010701", 	"Green Island",
  "NY010802", 	"Guilderland",
  "NY490601", 	"Lansingburgh",
  "NY010615", 	"Menands",
  "NY530301", 	"Niskayuna",
  "NY010623", 	"North Colonie",
  "NY010402", 	"Ravena-Coeymans-Selkirk",
  "NY491200", 	"Rensselaer",
  "NY530515", 	"Rotterdam (Mohonasen)",
  "NY530501",	  "Rotterdam (Schalmont)",
  "NY530600", 	"Schenectady",
  "NY491501", 	"Schodack",
  "NY530202", 	"Scotia Glenville",
  "NY520302", 	"Shenendehowa",
  "NY010601", 	"South Colonie",
  "NY491700", 	"Troy",
  "NY011200", 	"Watervliet",
  "NY490804",	  "Wynantskill"
), ncol = 2, byrow = T), stringsAsFactors = F)
DistrictNames = DORlookup$V2[match(DistrictCodes, DORlookup$V1)]
dimnames(LunchByDistrict)$DISTRICTCODEOFRESIDENCE = DistrictNames
write.csv(LunchByDistrict, "lunchByDistrict.csv")


#---------------------#
#### BEDS Day FRPL ####
#---------------------#

# This is useful for completing the IMF
# It requires that all FRPL records (from both DCMP and lunch forms) be entered in PowerSchool already.
x = EnrollmentExtract$SCHOOLEXITDATEENROLLMENTEXITDATE == ""               # which exist dates are missing?
EnrollmentExtract$SCHOOLEXITDATEENROLLMENTEXITDATE[x] = schoolYear("end")  # set missing exit dates to the end of the year
# convert exit date to date type
EnrollmentExtract$SCHOOLEXITDATEENROLLMENTEXITDATE = as.Date(EnrollmentExtract$SCHOOLEXITDATEENROLLMENTEXITDATE)
# convert entry date to date type
EnrollmentExtract$SCHOOLENTRYDATEENROLLMENTENTRYDATE = as.Date(EnrollmentExtract$SCHOOLENTRYDATEENROLLMENTENTRYDATE) 

y = EnrollmentExtract$SCHOOLENTRYDATEENROLLMENTENTRYDATE < BedsDate()   # which students were around before BEDS day?
y = y & EnrollmentExtract$SCHOOLEXITDATEENROLLMENTEXITDATE > BedsDate() # which students were around after BEDS day?
BEDSstudents = EnrollmentExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID[y]    # Get a vector of ID's of students enrolled on BEDS day

# subset lunch services to just students who were enrolled on BEDS day
bedsLunchServices = lunch[lunch$STUDENTIDSCHOOLDISTRICTSTUDENTID %in% BEDSstudents,] 
summary(factor(bedsLunchServices$PROGRAMSCODEPROGRAMSERVICECODE))                    # Summarize the results


#------------------------------------------#
#### Update the Accountability Workbook ####
#------------------------------------------#

# Only do this part if everything else is already updated

wkbkLunch = Workbook[,c("Local.ID.(optional)", "Cohort.Year.(year.1st.entered.9th)", "Lunch.Status")]
wkbkLunch = DFna.to.empty(wkbkLunch)

# For each student,
#   If the student has reduced or free lunch, mark it
#   If the student has neither AND is a student for this year, mark it as paid
for(i in 1:nrow(wkbkLunch)){
  currID = wkbkLunch$`Local.ID.(optional)`[i]
  if(currID %in% reducedLunch$STUDENTIDSCHOOLDISTRICTSTUDENTID){  
    wkbkLunch$Lunch.Status[i] = "R"
  } else if (currID %in% freeLunch$STUDENTIDSCHOOLDISTRICTSTUDENTID){ 
    wkbkLunch$Lunch.Status[i] = "F"
  } else if (currID %in% StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID){ 
    wkbkLunch$Lunch.Status[i] = "P"
  }
} # /for

write.csv(wkbkLunch, "Workbook Lunch Status.csv")

# Go paste the workbook lunch status into the workbook, one tab at a time

