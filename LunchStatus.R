# LunchStatus.R

# To run this, you need the following:
#   A recent download of the state matches from the NYSSIS website
#   A recent Program Facts extract from Power school
#   A recent Demographics extract from Power school
#   A recent Enrollment extract from Power school

# For certain parts, you may also need the following:
#   The spreadsheet of lunch forms from the main office (this should be in a shared google sheet)
#   An export of unmatched students from the powerschool student table
#   A download of the results of the bulk matching process from the NYSSIS website


# see instructions at https://docs.google.com/document/d/1pDkXNgq-2UbgPvolLHpde_sE2l4Cb2sUHOah0Ke3rbE/edit?usp=sharing

#-----------------#
#### Load Data ####
#-----------------#

powerschool = powerschoolraw

# get the file of state matches downloaded from the nyssis website
# You must open the file in excel, save it, and close it again before importing it here
# Make sure that it goes in the lunch folder for the current year, and begins with "nyssis_lunch"

# Load all of them from this year and then combine them into one data.frame
NyssisFile = read.xlsx.multi(folder = LunchLocation, pattern = "nyssis_lunch", dupVar = "Local.ID", orderBy = "Certification.Method")
NyssisSnap = NyssisFile[NyssisFile$Certification.Method == "SNAP",]
NyssisMedicaid = NyssisFile[NyssisFile$Certification.Method == "MEDICAID",]

# generate the enrollment, student lite, and program service extracts from PowerSchool and load them here
EnrollExt = read.csv(file = file.choose(), header = F, stringsAsFactors = F)
StudentLiteExtract = read.csv(file = file.choose(), header = F, stringsAsFactors = F)
progserv = read.csv(file = file.choose(), header = F, stringsAsFactors = F)

# Add column names to the SIRS extracts
colnames(progserv) = GetNiceColumnNames("PROGRAMS FACT", templates)
colnames(StudentLiteExtract) = GetNiceColumnNames("STUDENT LITE", templates)[1:ncol(StudentLiteExtract)]
colnames(EnrollExt) = GetNiceColumnNames("SCHOOL ENTRY EXIT", templates)
colnames(progserv)[colnames(progserv) == "STUDENTIDSCHOOLDISTRICTSTUDENTID"] = "StudentID"
colnames(StudentLiteExtract)[colnames(StudentLiteExtract) == "STUDENTIDSCHOOLDISTRICTSTUDENTID"] = "StudentID"
colnames(EnrollExt)[colnames(EnrollExt) == "STUDENTIDSCHOOLDISTRICTSTUDENTID"] = "StudentID"



# Get just the lunch services that are already in powerschool
lunch = progserv[progserv$PROGRAMSCODEPROGRAMSERVICECODE %in% c(5806, 5817),]
# Students who currently are set as Reduced Price lunch in powerschool
reducedLunch = lunch[lunch$PROGRAMSCODEPROGRAMSERVICECODE == 5806,]            
# Students who currently are set as Free lunch in powerschool
freeLunch = lunch[lunch$PROGRAMSCODEPROGRAMSERVICECODE == 5817,]               

# Get the Nyssis Matches
allMatches = NyssisFile[NyssisFile$Local.ID %in% StudentLiteExtract$StudentID,
                        c("Local.ID", "P12.First.Name","P12.Last.Name", "Certification.Method","Case.Numbers")]

# Load and manipulate the lunch form spreadsheet (if it exists)
statusTypes = data.frame(
  Status = c("F", "R", "D", "Free", "Reduced", "Denied", "Paid"), 
  Code = c("F", "R", "D", "F", "R", "D", "D"), 
  stringsAsFactors = F)
forms = read.xlsx(xlsxFile = file.choose()) # read in the Lunch Applications file from Ms Hood
forms = forms[!is.na(forms$Lunch.Form),]    # leave out the ones with no lunch form
forms = forms[!is.na(forms$Lunch.Status),]        # leave out the ones with no lunch status
forms$Status = statusTypes$Code[match(forms$Lunch.Status, statusTypes$Status)]
forms = forms[!is.na(forms$Status),]        # leave out the ones with no lunch status
forms = forms[forms$Status != "D",]         # leave out the ones who are Denied (paid lunch)


#-----------------------------------------------------#
#### Bulk Upload 1 - determining students to check ####
#-----------------------------------------------------#

# Only run this section if you plan to make a bulk upload to submit to the NYSSIS site
# This should only be done once per year

# Which of this year's students do not appear in the NYSSIS extract?
StudentsToCheck = setdiff(StudentLiteExtract$StudentID, NyssisFile$Local.ID)
StudentsToCheck.df = powerschool[powerschool$student_number %in% StudentsToCheck,]
StudentsToCheck.FilePath = paste0(LunchLocation, "/studentsToCheck (",Sys.Date(),").xlsx")
write.xlsx(x = StudentsToCheck.df, file = StudentsToCheck.FilePath)
CNMS.MakeBulkUpload(xlsxFile = StudentsToCheck.FilePath, singleGender = "M", uploadfilename = paste0(OutFolder, "bulkupload.txt"))
# Upload the file bulkupload.txt to the NYSSIS web interface


#-------------------------------------------------#
#### Bulk Upload 2 - Using Bulk Upload Matches ####
#-------------------------------------------------#

# Only do this next part if Bulk Upload process has completed
# Wait 24 hours after running Bulk Upload 1, then go back into NYSSIS.  
# Click on the number of records next to the upload.  
# In 19-20, they changed it so there are two numbers to click on.  You want the "Side By Side Match Candidates".
# Download the results in Excel format.
# Open the file, save it, and close it again.

bulkMatches = read.xlsx(xlsxFile = paste0(LunchLocation,"/Child Nutrition - Bulk Search Results (2020-10-01).xlsx"))
bulkMatches$P12.First.Name = powerschool$First_name[match(bulkMatches$Local.ID, powerschool$student_number)] 
bulkMatches$P12.Last.Name = powerschool$Last_Name[match(bulkMatches$Local.ID, powerschool$student_number)] 
bulkMatches = bulkMatches[,c("Local.ID", "P12.First.Name", "P12.Last.Name", "Certification.Method", "Case.Number(s)")]
colnames(bulkMatches) = colnames(allMatches)
allMatches = rbindlist(l = list(allMatches, bulkMatches))
allMatches = allMatches[order(allMatches$Certification.Method, decreasing = T),]
allMatches = allMatches[!duplicated(allMatches$Local.ID),]
summary(allMatches)
summary(factor(allMatches$Certification.Method))
write.csv(x = allMatches, file = paste0(OutFolder, "allmatches.csv"))

unmatchedIDs = setdiff(StudentLiteExtract$StudentID, allMatches$Local.ID)                              # ID's of unmatched students 
unmatchedIDs = StudentLiteExtract$StudentID %in% unmatchedIDs                                          # Logical, which rows have unmatched students
write.csv(x = StudentLiteExtract[unmatchedIDs, 4:8], file = paste0(OutFolder,"unmatchedStudents.csv")) # output unmatched students

# Inform the relevant people about the number of matches.
# Send the two csv's to whoever needs them.
# The students in unmatchedStudents.csv need to submit lunch forms


#----------------------------------------------------#
#### PowerSchool Upload and Changes based on DCMP ####
#----------------------------------------------------#

# Get set of students for new upload
newmatches = allMatches[!(allMatches$Local.ID %in% lunch$StudentID),]
if(nrow(newmatches) > 0){
  print("see the newmatches.csv file")
  print("Use the program service upload template to import these into PowerSchool.")
  print("For the eligibility code, use 'DCMP'.")
  write.csv(newmatches, paste0(OutFolder,"newmatches.csv"))
} else {
  print("There are no new matches to upload to PowerSchool")
} # /if-else


# Students who are set as reduced price lunch, but should be set as free lunch
reducedToSwitch = allMatches$Local.ID %in% reducedLunch$StudentID
if(sum(reducedToSwitch) > 0){
  print("The following students are set as Reduced Price lunch, but need to be changed to Free Lunch.")
  print(allMatches[reducedToSwitch,])
} else {
  print("There are no students who need to be changed from Reduced Price Lunch to Free Lunch in PowerSchool")
} # /if-else


# Students who currently have free lunch, but not the DCMP eligibility code
if(nrow(freeLunch) > 0){
  freeLunch$DCMP = FALSE        # Initialize the DCMP variable
  for(i in 1:nrow(freeLunch)){  # Determine which students have the DCMP code
    freeLunch$DCMP[i] = any(freeLunch[i,paste0("PROGRAMELIGIBILITYCODE",1:6)] == "DCMP")
  } # /for
  freeAddDCMP = freeLunch[!freeLunch$DCMP,] # Subset to just those who don't have the DCMP code
  # Find students with free lunch in powerschool who need the DCMP code
  freeAddDCMP = allMatches$Local.ID %in% freeAddDCMP$StudentID  
  if(sum(freeAddDCMP) > 0){
    print("The following students are set as Free lunch, but need to have DCMP added to their program eligibility codes.")
    print(allMatches[freeAddDCMP,])
  } else {
    print("There are no Free Lunch students in PowerSchool that need to have DCMP added to their program eligibility codes.")
  } # /if-else
} else {
  print("There are no Free Lunch records in Powerschool.  None at all.")
}


#-----------------------------------------------#
#### PowerSchool Upload based on Lunch Forms ####
#-----------------------------------------------#

# Only do this part if there is lunch form data
forms.new = forms[!(forms$Student.Number %in% allMatches$Local.ID),]     # leave out the ones who are already in allMatches
forms.new = forms.new[!(forms.new$Student.Number %in% lunch$StudentID),] # leave out the ones who are already in PowerSchool
if(nrow(forms.new) > 0){
  write.csv(forms.new, paste0(OutFolder, "newFormMatches.csv"))
  print("There are new matches to upload")
  print("see the file newFormMatches.csv")
  print("Use the program service upload template to import these into PowerSchool.")
  print("For the eligibility code, use 'Application'.")
} else {
  print("No new matches from the forms")
} # /if-else




#----------------------------------------#
#### Check for other reasons for FRPL ####
#----------------------------------------#

homeless = progserv[progserv$PROGRAMSCODEPROGRAMSERVICECODE %in% c(8262, 8272, 0892),]

if(nrow(homeless) > 0){
  fine = 0
  homelessStudents = homeless$StudentID
  homelessReducedLunch = reducedLunch$StudentID[reducedLunch$StudentID %in% homelessStudents]
  homelessFreeLunch = freeLunch[freeLunch$StudentID %in% homelessStudents,]
  homelessAllLunch = unique(c(homelessReducedLunch, homelessFreeLunch$StudentID))
  homelessNoLunch = setdiff(homelessStudents, homelessAllLunch)
  homelessFreeLunch$HomelessEligibility = F
  for(i in 1:nrow(homelessFreeLunch)){
    homelessFreeLunch$HomelessEligibility[i] = betterGrepl.any(
      pattern = "HOMELESS", 
      x = as.character(homelessFreeLunch[i,paste0("PROGRAMELIGIBILITYCODE", 1:6)]))
  }
  homelessFreeLunch = homelessFreeLunch$StudentID[!homelessFreeLunch$HomelessEligibility]
  
  if(length(homelessNoLunch) > 0){
    print("The following students need free lunch service added with the HOMELESS eligibility code.")
    print(homelessNoLunch)
  } else { fine = fine + 1}
  
  if(length(homelessReducedLunch) > 0){
    print(paste0("the following students have reduced price lunch program service, ",
                 "but should have free lunch with the HOMELESS eligibility code."))
    print(homelessReducedLunch)
  } else { fine = fine + 1}
  
  if(length(homelessFreeLunch) > 0){
    print("The following students have free lunch service but need the HOMELESS eligibility code.")
    print(homelessFreeLunch)
  } else {fine = fine + 1}
  
  if(fine == 3){
    print("No issues were found where students need lunch status changes due to homelessness.")
  }
}




foster = progserv[progserv$PROGRAMSCODEPROGRAMSERVICECODE %in% c(8300),]
if(nrow(foster) > 0){
  fine = 0
  fosterStudents = foster$StudentID
  fosterReducedLunch = reducedLunch$StudentID[reducedLunch$StudentID %in% fosterStudents]
  fosterFreeLunch = freeLunch[freeLunch$StudentID %in% fosterStudents,]
  fosterAllLunch = unique(c(fosterReducedLunch, fosterFreeLunch$StudentID))
  fosterNoLunch = setdiff(fosterStudents, fosterAllLunch)
  fosterFreeLunch$fosterEligibility = F
  for(i in 1:nrow(fosterFreeLunch)){
    fosterFreeLunch$fosterEligibility[i] = betterGrepl.any(
      pattern = "FOSTER", 
      x = as.character(fosterFreeLunch[i,paste0("PROGRAMELIGIBILITYCODE", 1:6)]))
  }
  fosterFreeLunch = fosterFreeLunch$StudentID[!fosterFreeLunch$fosterEligibility]
  
  if(length(fosterNoLunch) > 0){
    print("The following students need free lunch service added with the FOSTER eligibility code.")
    print(fosterNoLunch)
  } else { fine = fine + 1}
  
  if(length(fosterReducedLunch) > 0){
    print(paste0("the following students have reduced price lunch program service, ",
                 "but should have free lunch with the FOSTER eligibility code."))
    print(fosterReducedLunch)
  } else { fine = fine + 1}
  
  if(length(fosterFreeLunch) > 0){
    print("The following students have free lunch service but need the FOSTER eligibility code.")
    print(fosterFreeLunch)
  } else {fine = fine + 1}
  
  if(fine == 3){
    print("No issues were found where students need lunch status changes due to fostering.")
  }
  
}

migrant = progserv[progserv$PROGRAMSCODEPROGRAMSERVICECODE %in% c(0330),]
if(nrow(migrant) > 0){
  print("Do something with the migrant students.")
}






#-----------------------------#
#### Breakdown by District ####
#-----------------------------#

# This section is useful for the annual Title 1 report that gets submitted through a web form
# It requires that program services already be loaded in PowerSchool

StudentLiteExtract$EnrolledOnBEDSDay = F
StudentLiteExtract$EnrolledByBEDSDay = F
EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE[EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE == ""] = as.character(schoolYear("end"))

BEDSday = BedsDate()

for(i in 1:nrow(StudentLiteExtract)){
  enrollRows = EnrollExt$StudentID == StudentLiteExtract$StudentID[i]
  enrollments = EnrollExt[enrollRows,]
  for(j in 1:nrow(enrollments)){
    enrollments$Before[j] = enrollments$SCHOOLENTRYDATEENROLLMENTENTRYDATE[j] <= BEDSday
    enrollments$After[j]  = enrollments$SCHOOLEXITDATEENROLLMENTEXITDATE[j] >= BEDSday
    enrollments$Both[j] = enrollments$Before[j] & enrollments$After[j]
  } # /for each of that student's enrollment records
  StudentLiteExtract$EnrolledByBEDSDay[i] = any(enrollments$Before)
  StudentLiteExtract$EnrolledOnBEDSDay[i] = any(enrollments$Both)
} # /for each student

# This next section can only work if lunch services are already in PowerSchool
StudentLiteExtract$Lunch = factor(
  lunch$PROGRAMSCODEPROGRAMSERVICECODE[match(
    StudentLiteExtract$StudentID, 
    lunch$StudentID)])
summary(StudentLiteExtract$Lunch)

# This next section will work as long as the bulk match process has already been done
# StudentLiteExtract$Lunch = "Paid"
# StudentLiteExtract$Lunch[StudentLiteExtract$StudentID %in% allMatches$Local.ID] = "Free"
# StudentLiteExtract$Lunch = as.factor(StudentLiteExtract$Lunch)

# Use the following line to determine which students to use
# Subset it by EnrolledOnBEDSDay or EnrolledByBEDSDay, if desired.
# Otherwise, don't subset it at all and just use the whole StudentLiteExtract
usableStudents = StudentLiteExtract[StudentLiteExtract$EnrolledOnBEDSDay,]

LunchByDistrict = table(usableStudents[,c("Lunch", "DISTRICTCODEOFRESIDENCE")], useNA = "always")
DistrictCodes = dimnames(LunchByDistrict)$DISTRICTCODEOFRESIDENCE
DistrictNames = DORs$District.Name[match(DistrictCodes, DORs$District.ID)]
dimnames(LunchByDistrict)$DISTRICTCODEOFRESIDENCE = DistrictNames
write.csv(LunchByDistrict, paste0(OutFolder,"lunchByDistrict.csv"))

LunchByGradeLevel = table(usableStudents[,c("Lunch", "CURRENTGRADELEVELGRADELEVEL")])
write.csv(LunchByGradeLevel, paste0(OutFolder,"LunchByGradeLevel.csv"))

# Preliminary Title 1 predictions for May 1 report #
usableStudents$DOR = DORs$District.Name[match(usableStudents$DISTRICTCODEOFRESIDENCE, DORs$District.ID)]
usableStudents.9to11 = usableStudents[usableStudents$CURRENTGRADELEVELGRADELEVEL < 12,]
usableStudents.9 = usableStudents[usableStudents$CURRENTGRADELEVELGRADELEVEL == 9,]
usableStudents.projection = rbind.data.frame(usableStudents.9, usableStudents.9to11)
LunchByDistrictPrediction = table(usableStudents.projection[,c("Lunch", "DOR")], useNA = "always")
write.csv(LunchByDistrictPrediction, paste0(OutFolder,"LunchByDistrictPrediction.csv"))

#---------------------#
#### BEDS Day FRPL ####
#---------------------#

# This is useful for completing the IMF
# It requires that all FRPL records (from both DCMP and lunch forms) be entered in PowerSchool already.
x = EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE == ""                             # which exit dates are missing?
EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE[x] = as.character(schoolYear("end"))  # set missing exit dates to the end of the year
# convert exit date to date type
EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE = as.Date(EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE)
# convert entry date to date type
EnrollExt$SCHOOLENTRYDATEENROLLMENTENTRYDATE = as.Date(EnrollExt$SCHOOLENTRYDATEENROLLMENTENTRYDATE) 

y = EnrollExt$SCHOOLENTRYDATEENROLLMENTENTRYDATE < BedsDate()   # which students were around before BEDS day?
y = y & EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE > BedsDate() # which students were around after BEDS day?
bedsStudents = EnrollExt$StudentID[y]                           # Get a vector of ID's of students enrolled on BEDS day

# subset lunch services to just students who were enrolled on BEDS day
bedsLunchServices = lunch[lunch$StudentID %in% bedsStudents,] 
summary(factor(bedsLunchServices$PROGRAMSCODEPROGRAMSERVICECODE))   # Summarize the results
length(bedsStudents) - nrow(bedsLunchServices)







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
  if(currID %in% reducedLunch$StudentID){  
    wkbkLunch$Lunch.Status[i] = "R"
  } else if (currID %in% freeLunch$StudentID){ 
    wkbkLunch$Lunch.Status[i] = "F"
  } else if (currID %in% StudentLiteExtract$StudentID){ 
    wkbkLunch$Lunch.Status[i] = "P"
  }
} # /for

write.csv(wkbkLunch, paste0(OutFolder, "Workbook Lunch Status.csv"))

# Go paste the workbook lunch status into the workbook


#----------------------------#
#### Cafeteria Info Sheet ####
#----------------------------#

# This is for creating a list for the cafeteria staff to include in a binder
# This requires that the Nyssis info, powerschool info, and lunch form spreadsheet be up to date
CafeList = StudentLiteExtract[,c("StudentID", "LASTNAMESHORTSTUDENTSLASTNAME", "FIRSTNAMESHORTSTUDENTSFIRSTNAME", 
                                 "CURRENTGRADELEVELGRADELEVEL", "BIRTHDATEDATEOFBIRTH", "GENDERCODEGENDERDESCRIPTION")]
CafeList$Status = "Paid"
for(i in 1:nrow(CafeList)){
  currID = CafeList$StudentID[i]
  if(currID %in% reducedLunch$StudentID){  
    wkbkLunch$Lunch.Status[i] = "Reduced"
  } else if (currID %in% freeLunch$StudentID){ 
    wkbkLunch$Lunch.Status[i] = "Free"
  }
}

CafeList$Medicaid = F
CafeList$SNAP = F
CafeList$Form = F
CafeList$MedicaidCaseNo = ""
CafeList$SnapCaseNo = ""
for(i in 1:nrow(CafeList)){
  print(i)
  currID = CafeList$StudentID[i]
  if(currID %in% NyssisMedicaid$Local.ID){
    CafeList$Medicaid[i] = T
    casenumbers = NyssisMedicaid$Case.Numbers[NyssisMedicaid$Local.ID == currID]
    if(length(casenumbers) > 1){
      stop("Multiple medicaid case numbers")
    }
    CafeList$MedicaidCaseNo[i] = casenumbers
  }
  if(currID %in% NyssisSnap$Local.ID){
    CafeList$SNAP[i] = T
    casenumbers = NyssisSnap$Case.Numbers[NyssisSnap$Local.ID == currID]
    if(length(casenumbers) > 1){
      stop("Multiple snap case numbers")
    }
    CafeList$SnapCaseNo[i] = casenumbers
  }
  if(currID %in% forms$Student.Number){
    CafeList$Form = T
  }
}

colnames(CafeList) = c("STUDENT.ID", "LAST.NAME", "FIRST.NAME", "GRADE.LEVEL", "DOB", "GENDER", 
                       "Lunch.Status", "Medicaid", "SNAP", "Form", "MedicaidCaseNo", "SnapCaseNo")
write.csv(CafeList, "list for cafeteria.csv")


#---------------------------------------#
#### SNAP & Medicaid Certified Entry ####
#---------------------------------------#

oct1 = Oct1()
y = EnrollExt$SCHOOLENTRYDATEENROLLMENTENTRYDATE < oct1              # which students were around before october 1?
septStudents = EnrollExt$StudentID[y]                                # Get a vector of ID's of students enrolled before oct 1

SnapMedCertEntry = data.frame(StudentNumber = as.character(septStudents), stringsAsFactors = F)
SnapMedCertEntry$SNAP = F
SnapMedCertEntry$Medicaid = F
for(i in 1:nrow(SnapMedCertEntry)){
  if(SnapMedCertEntry$StudentNumber[i] %in% NyssisSnap$Local.ID){
    SnapMedCertEntry$SNAP[i] = T
  }
  if(SnapMedCertEntry$StudentNumber[i] %in% NyssisMedicaid$Local.ID){
    SnapMedCertEntry$Medicaid[i] = T
  }
}

SnapMedCertEntry$Category = "None"
SnapMedCertEntry$Category[SnapMedCertEntry$Medicaid] = "Medicaid"
SnapMedCertEntry$Category[SnapMedCertEntry$SNAP] = "SNAP"

summary(factor(SnapMedCertEntry$Category))


#-------------------------------------------#
#### CEP - Community Eligibility Program ####
#-------------------------------------------#

# This should be based on the students enrolled on April 1st

# Determine all students who were enrolled on April 1st
# For each one, use the eligibility codes in the program service records to classify each student
# For DCMP, break down by SNAP vs Medicaid
# Actually, it would be better to just have a column for each classification and enter it as True or False
# Then, all of that can be exported and modified as necessary for other things not included in data herein

dateVars = c("SCHOOLEXITDATEENROLLMENTEXITDATE", "SCHOOLENTRYDATEENROLLMENTENTRYDATE")
Enroll.CEP = EnrollExt[,c("StudentID", dateVars)]


for(i in dateVars){
  Enroll.CEP[,i][Enroll.CEP[,i] == ""] = NA
  Enroll.CEP[,i] = xlDate(Enroll.CEP[,i]) 
}

colnames(Enroll.CEP)[colnames(Enroll.CEP) %in% dateVars] = c("Exit", "Entry")

Enroll.CEP$Exit[is.na(Enroll.CEP$Exit)] = schoolYear(x = "end")

april1 = as.Date(paste0(schoolYear()+1, "-04-01"))
Enroll.CEP$OnApril1 = Enroll.CEP$Exit >= april1 & Enroll.CEP$Entry <= april1
Enroll.CEP = Enroll.CEP[Enroll.CEP$OnApril1,]
Enroll.CEP$LastName = StudentLiteExtract$LASTNAMESHORTSTUDENTSLASTNAME[match(Enroll.CEP$StudentID, StudentLiteExtract$StudentID)]
Enroll.CEP$FirstName = StudentLiteExtract$FIRSTNAMESHORTSTUDENTSFIRSTNAME[match(Enroll.CEP$StudentID, StudentLiteExtract$StudentID)]
Enroll.CEP$GradeLevel = StudentLiteExtract$CURRENTGRADELEVELGRADELEVEL[match(Enroll.CEP$StudentID, StudentLiteExtract$StudentID)]

fields = c("SNAP", "ExtensionOfEligibility", "Foster", "Homeless", "Migrant", "Runaway", "HeadStart", "Medicaid")
Enroll.CEP[,fields] = F


for(i in 1:nrow(Enroll.CEP)){
  curID = Enroll.CEP$StudentID[i]
  Enroll.CEP$SNAP[i] = as.character(curID) %in% NyssisSnap$Local.ID
  Enroll.CEP$Foster[i] = curID %in% foster$StudentID
  Enroll.CEP$Homeless[i] = curID %in% homeless$StudentID
  Enroll.CEP$Medicaid[i] = as.character(curID) %in% NyssisMedicaid$Local.ID
}


summary(Enroll.CEP[,fields])
write.csv(x = Enroll.CEP, file = paste0(OutFolder, "CEP export.csv"))

