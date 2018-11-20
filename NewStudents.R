# NewStudents.R 

# This script does some stuff with the application tracker.
# The first part determines the school districts
# The second part creates the powerschool import files


####              NOTE!!!           ####
# Before doing an upload, compare new students to existing students to see if there are any repeat applicants.




#--------------------------------------------------#
#### Pull the data from the Application Tracker ####
#--------------------------------------------------#
gs_auth()
appSheet_gs = gs_url(x = "https://docs.google.com/spreadsheets/d/1mLuUQIFArmhvUMCYG0JciOZbeckvindxCloSj_q93TE/edit?usp=sharing")
appSheet = gs_read(ss = appSheet_gs, ws = "Tracker", range = "A2:BC300")
appSheet = appSheet[!is.na(appSheet$Status),]


appSheet$FullAddress = paste0(appSheet$Address, ", ", appSheet$City, ", ", "NY", " ", appSheet$Zip)
appSheet$FullAddress = gsub(pattern = "#", replacement = "", x = appSheet$FullAddress, fixed = T)
appSheet$FullAddress2 = paste0(appSheet$`Address 2`, ", ", appSheet$`City 2`, ", ", "NY", " ", appSheet$`Zip 2`)
appSheet$FullAddress2[is.na(appSheet$`Address 2`)] = NA_character_
appSheet$CensusDistrict = ""


#-------------------------------------#
#### Check for phone number errors ####
#-------------------------------------#

phoneVars = c("Home 1", "Cell 1", "Work 1", "Home 2", "Cell 2", "Work 2")
phoneNums = unlist(c(appSheet[,phoneVars]))
phoneNums = phoneNums[!is.na(phoneNums)]

grep(pattern = "^[[:digit:]]{3}-[[:digit:]]{3}-[[:digit:]]{4}$", x = phoneNums, value = T, invert = T)  


#--------------------------------------------------------#
#### Query the Census API to get the school districts ####
#--------------------------------------------------------#

for(i in 1:nrow(appSheet)){
  print(paste0(i, " of ", nrow(appSheet)))
  appSheet$CensusDistrict[i] = getSchoolDistrict(addr = appSheet$FullAddress[i], halt = F)
}

# Test an address using the next line
# getSchoolDistrict("181 North Lake Ave (Apt. 2), Albany, NY 12206", halt = F)


#----------------------------------------------------------#
#### Examine the results, comparing to the entered DORs ####
#----------------------------------------------------------#

appSheet$CenDistShort = substr(appSheet$CensusDistrict, 1, 20)
table(as.data.frame(appSheet[,c("CenDistShort", "DOR Name")]), useNA = "ifany")


#----------------------------------------------------------#
#### Generate output for the addresses that didn't work ####
#----------------------------------------------------------#

DORlookup = SWSM(gs_read(ss = appSheet_gs, ws = "Lists", range = "C2:F27", verbose = F))
appSheet$DOR2 = DORlookup$`Census Name`[match(appSheet$`DOR Name`, DORlookup$Name)]
appSheet.limited = appSheet[!VbetterComp(appSheet$DOR2, appSheet$CensusDistrict), c("Student ID", "FullAddress", "FullAddress2", "CensusDistrict", "DOR2")]
write.csv(x = appSheet.limited, file = paste0(OutFolder,"output.csv"))


#----------------------------------------------------------------#
#### Make a data.frame of accepted students and add race info ####
#----------------------------------------------------------------#

accepted = as.data.frame(appSheet[appSheet$Status == "Accepted",])
rownames(accepted) = NULL
accepted$RepeaterFlag = accepted$`Repeater?` == "*"

accepted$racecount = 0
for(i in 1:nrow(accepted)){
  accepted$racecount[i] = 5 - sum(is.na(accepted[i,c("Black?", "White?", "Asian?", "Native American?", "Pacific Islander?")]))
}

accepted$Ethnicity = ""
accepted$Ethnicity[VbetterComp(accepted$`Black?`, "Y")] = "B"
accepted$Ethnicity[VbetterComp(accepted$`White?`, "Y")] = "W"
accepted$Ethnicity[VbetterComp(accepted$`Pacific Islander?`, "Y")] = "P"
accepted$Ethnicity[VbetterComp(accepted$`Asian?`, "Y")] = "A"
accepted$Ethnicity[VbetterComp(accepted$`Native American?`, "Y")] = "N"
accepted$Ethnicity[accepted$racecount > 1] = "M"
accepted$Ethnicity[VbetterComp(accepted$`Hispanic?`, "Y")] = "H"

#---------------------------------------------------------------#
#### Make the flat file to import into the PS Students table ####
#---------------------------------------------------------------#

imp.Stu = data.frame(Student_Number = accepted$`Student ID`)

suffixes = accepted$Suffix
suffixes[!is.na(suffixes)] = paste0(" ", suffixes[!is.na(suffixes)])
suffixes[is.na(suffixes)] = ""
imp.Stu$Last_Name = paste0(accepted$`Student's Last Name`, suffixes)

imp.Stu$First_Name = accepted$`Student's First Name`

midnames = accepted$`Student's Middle Name`
midnames[is.na(midnames)] = ""
imp.Stu$Middle_Name = midnames

DOBs = accepted$`Date of Birth`
DOBs = gsub(pattern = "^([[:digit:]]/)", replacement = "0\\1", x = DOBs)
DOBs = gsub(pattern = "(/)([[:digit:]]/)", replacement = "/0\\2", x = DOBs)
imp.Stu$DOB = DOBs

imp.Stu$Grade_Level = accepted$`Grade Applying For`  

imp.Stu$Home_Phone = accepted$`Home 1`

imp.Stu$Street = accepted$Address
imp.Stu$City = accepted$City
imp.Stu$Zip = accepted$Zip
imp.Stu$State = "NY"

imp.Stu$DistrictOfResidence = accepted$`DOR Code`

hispanic = accepted$`Hispanic?`
hispanic[hispanic == "Y"] = "1"
hispanic[is.na(hispanic)] = "0"
imp.Stu$FedEthnicity = hispanic

imp.Stu$Gender = "M"

imp.Stu$Enroll_Status = "0"
imp.Stu$EntryCode = "0011"
imp.Stu$EntryDate = "08/21/2018"
imp.Stu$ExitDate = "06/29/2019"
imp.Stu$TransferComment = paste0("Enrolled Fall ", schoolYear())
imp.Stu$SchoolEntryDate = "08/21/2018" 

imp.Stu$SchoolID = "100860907"
imp.Stu$Enrollment_SchoolID = "100860907"
imp.Stu$Next_School = "100860907"

imp.Stu$Sched_Scheduled = "1"

imp.Stu$Sched_YearOfGraduation = schoolYear() + 13 - imp.Stu$Grade_Level

imp.Stu$Sched_NextYearGrade = imp.Stu$Grade_Level + 1

imp.Stu$Ethnicity = accepted$Ethnicity

imp.Stu$SchoolEntryGradeLevel = imp.Stu$Grade_Level
imp.Stu$Track = "A"

imp.Stu$Mailing_City = imp.Stu$City
imp.Stu$Mailing_State = imp.Stu$State
imp.Stu$Mailing_Street = imp.Stu$Street
imp.Stu$Mailing_Zip = imp.Stu$Zip

imp.Stu$Lunch_ID = substr(imp.Stu$Student_Number, 5, 9)

imp.Stu$S_NY_STU_X.DateOfEntryGrade9 = "08/21/2018"
imp.Stu$S_NY_STU_X.DateOfEntryGrade9[imp.Stu$Grade_Level != 9] = ""
imp.Stu$S_NY_STU_X.DateOfEntryGrade9[imp.Stu$RepeaterFlag] = ""

write.psimport(x = imp.Stu, file = paste0(OutFolder, "StudentImport.txt"))


#------------------------------------------------------------------#
#### Make the flat file to import into the PS StudentRace table ####
#------------------------------------------------------------------#

# Note: The Students tab in the PowerSchool excel workbook must already have the new students in it for this to work

raceColumns = c("Black?", "White?", "Asian?", "Native American?", "Pacific Islander?")
imp.Race = accepted[,c("Student ID", raceColumns)]
imp.Race$StudentID = powerschoolraw$ID[match(imp.Race$`Student ID`, powerschoolraw$student_number)]

if(nrow(imp.Race[is.na(imp.Race$StudentID),]) > 0){
  print("Warning!  There are accepted students in the tracker who are not in PowerSchool!")
  print("")
  print(imp.Race[is.na(imp.Race$StudentID),])
}

imp.Race = imp.Race[!is.na(imp.Race$StudentID),] # remove rows for students not in PowerSchool
imp.Race = imp.Race[!apply(X = is.na(imp.Race[,raceColumns]), MARGIN = 1, FUN = all),] # remove rows with no race
imp.Race = melt(data = imp.Race, id.vars = c("Student ID", "StudentID"))
imp.Race = imp.Race[!is.na(imp.Race$value),]
imp.Race$RaceCd = NA_character_
imp.Race$RaceCd[imp.Race$variable == "Black?"] = "B"
imp.Race$RaceCd[imp.Race$variable == "White?"] = "W"
imp.Race$RaceCd[imp.Race$variable == "Native American?"] = "N"
imp.Race$RaceCd[imp.Race$variable == "Pacific Islander?"] = "P"
imp.Race$RaceCd[imp.Race$variable == "Asian?"] = "A"

imp.Race = imp.Race[,c("StudentID", "RaceCd")]

write.psimport(x = imp.Race, file = paste0(OutFolder, "RaceImport.txt"))


#---------------------------------------------------------------#
#### Make the flat file to import into the PS Contacts table ####
#---------------------------------------------------------------#

stu.accepted = accepted
stu.accepted$StudentID = powerschoolraw$ID[match(stu.accepted$`Student ID`, powerschoolraw$student_number)]

if(nrow(stu.accepted[is.na(stu.accepted$StudentID),]) > 0){
  print("Warning!  There are accepted students in the tracker who are not in PowerSchool!")
  print("")
  print(stu.accepted[is.na(stu.accepted$StudentID),])
}

stu.accepted = stu.accepted[!is.na(stu.accepted$StudentID),] # remove rows for students not in PowerSchool

stu.accepted$contactLivesWith1 = "1"
stu.accepted$contactLivesWith2 = NA_character_
for(i in 1:nrow(stu.accepted)){
  if(is.na(stu.accepted$`Address 2`[i])){
    if(!is.na(stu.accepted$`Parent/\nGuardian 2`[i])){
      stu.accepted$contactLivesWith2[i] = "1"
    } 
  }
}

stu.accepted$addressPriorityOrder1 = "1"
stu.accepted$addressPriorityOrder2 = NA_character_
stu.accepted$addressPriorityOrder2[!is.na(stu.accepted$`Address 2`)] = "1"
stu.accepted$addressPriorityOrder2[stu.accepted$`Address 2` == ""] = ""


# Determine full set of unique Contact Persons

set1Vars = c("Student ID", "Parent/\nGuardian 1", "Email address 1", "Home 1", "Cell 1", "Work 1", 
             "Relationship 1", "Address", "City", "Zip", "Gender 1", "contactLivesWith1", "addressPriorityOrder1")

set2Vars = c("Student ID", "Parent/\nGuardian 2", "Email address 2", "Home 2", "Cell 2", "Work 2", 
             "Relationship 2", "Address 2", "City 2", "Zip 2", "Gender 2", "contactLivesWith2", "addressPriorityOrder2",
             "Parent 2 also has custody?", "Parent 2 can pick up student from school?", "Mail to address 2 also?")


set1 = stu.accepted[,c(set1Vars)] # 1st parent/guardian
set2 = stu.accepted[,c(set2Vars)] # 2nd parent/guardian

set1$hascustody = 1
set1$canpickup = 1
set1$getmail = 1
set2$`Mail to address 2 also?`[set2$`Mail to address 2 also?` == "No"] = NA
set2$`Mail to address 2 also?`[set2$`Mail to address 2 also?` == "Yes"] = "1"


colnames(set1) = c("StudentID", "ContactName", "Email", "Home", "Cell", "Work", 
                   "Relationship", "Address", "City", "Zip", "Gender", "contactLivesWith", 
                   "addressPriorityOrder", "hascustody", "canpickup", "getmail")
colnames(set2) = c("StudentID", "ContactName", "Email", "Home", "Cell", "Work", 
                   "Relationship", "Address", "City", "Zip", "Gender", "contactLivesWith", 
                   "addressPriorityOrder", "hascustody", "canpickup", "getmail")




# The next lines remove blank rows from set2 (the second parent/guardian)
set2$Drop = NA
for(i in 1:nrow(set2)){
  set2$Drop[i] = all(is.na(set2[i,2:ncol(set2)]))
}
set2 = set2[!set2$Drop,]
set2$Drop = NULL


set2$hascustody[set2$hascustody == "No"] = NA
set2$hascustody[set2$hascustody == "Yes"] = "1"
set2$canpickup[set2$canpickup == "No"] = NA
set2$canpickup[set2$canpickup == "Yes"] = "1"


setcombo = rbind(set1, set2) # Combine 1st and 2nd parent/guardian stuff

# The next line checks for duplicate contacts
if(length(setcombo$ContactName[duplicated(setcombo$ContactName)]) > 0){
  print(paste0("There are duplicate parents.  You will have to deal with that.  Here are the names:"))
  print(setcombo$ContactName[duplicated(setcombo$ContactName)])
} else {
  print("Yay! No duplicate contact names.")
  setcombo$identifier = 1:nrow(setcombo)
  setcombo$isActive = 1
}



phonecols = apply(X = expand.grid("phone", c("num", "typ", "NumberPriorityOrder"), 1:3), MARGIN = 1, FUN = paste0, collapse = "")
phonecols = c(phonecols, apply(X = expand.grid(c("isPreferred", "isSMS"), 1:3), MARGIN = 1, FUN = paste0, collapse = ""))

setcombo[,phonecols] = NA_character_


for(i in 1:nrow(setcombo)){
  numbersfound = 0
  numbers = setcombo[i,c("Home", "Cell", "Work")]
  numbers = as.data.frame(t(numbers), stringsAsFactors = F)
  numbers$type = rownames(numbers)
  numbers = numbers[!is.na(numbers[,1]),]
  if(nrow(numbers) > 0){
    numbers$phoneNumberPriorityOrder = 1:nrow(numbers)
    numbers$isPreferred = 0
    numbers$isPreferred[1] = 1
    numbers$isSMS = 0
    for(j in 1:nrow(numbers)){
      setcombo[i,paste0("phonenum",j)] = numbers[j,1]
      setcombo[i,paste0("phonetyp",j)] = numbers[j,2]
      setcombo[i,paste0("phoneNumberPriorityOrder",j)] = numbers[j,3]
      setcombo[i,paste0("isPreferred",j)] = numbers[j,4]
      setcombo[i,paste0("isSMS",j)] = numbers[j,5]
    }
  }
}


setcombo[,c("Home", "Cell", "Work")] = NULL
dropvars = c("Email", "Address", "City", "Zip", "Gender", "isActive", "ContactName")
row1 = setcombo
row2 = setcombo[!is.na(setcombo$phonenum2),]
row3 = setcombo[!is.na(setcombo$phonenum3),]

row1$phonenumber = row1$phonenum1
row1$phonetyp = row1$phonetyp1
row1$phoneNumberPriorityOrder = row1$phoneNumberPriorityOrder1
row1$isPreferred = row1$isPreferred1
row1$isSMS = row1$isSMS1

row2$phonenumber = row2$phonenum2
row2$phonetyp = row2$phonetyp2
row2$phoneNumberPriorityOrder = row2$phoneNumberPriorityOrder2
row2$isPreferred = row2$isPreferred2
row2$isSMS = row2$isSMS2
row2$addressPriorityOrder = ""
row2[,dropvars] = NA


row3$phonenumber = row3$phonenum3
row3$phonetyp = row3$phonetyp3
row3$phoneNumberPriorityOrder = row3$phoneNumberPriorityOrder3
row3$isPreferred = row3$isPreferred3
row3$isSMS = row3$isSMS3
row3$addressPriorityOrder = ""
row3[,dropvars] = NA

setcombo2 = rbind(row1, row2, row3)
setcombo2[,phonecols] = NULL
setcombo2$phonetyp[VbetterComp(setcombo2$phonetyp, "Cell")] = "Mobile"
setcombo2$extension = NA_character_

duplicateNumbers = setcombo2$phonenumber[duplicated(setcombo2$phonenumber) & !is.na(setcombo2$phonenumber)]
if(length(duplicateNumbers) > 0){
  print(paste0("There are duplicate phone numbers.  You might have to deal with that.  Here are the numbers:"))
  duplicateEntries = setcombo2[setcombo2$phonenumber %in% duplicateNumbers,]
  duplicateEntries = duplicateEntries[order(duplicateEntries$phonenumber),]
  write.csv(x = duplicateEntries, file = paste0(OutFolder, "duplicate phone numbers.csv"))
  print(duplicateEntries)
} else {
  print("Yay! No duplicate contact phone numbers.")
}

setcombo2$prefix = NA_character_
setcombo2$prefix[setcombo2$Gender == "M"] = "Mr."
setcombo2$prefix[setcombo2$Gender == "F"] = "Ms."

setcombo2$emailType[!is.na(setcombo2$Email)] = "Current"
setcombo2$isPrimaryEmailAddress[!is.na(setcombo2$Email)] = "1"
setcombo2$isEmergencyContact = 1

setcombo2$firstName = NA_character_
setcombo2$middleName = NA_character_
setcombo2$lastName = NA_character_
setcombo2$suffix = NA_character_
setcombo2$FixName = ""

for(i in 1:nrow(setcombo2)){
  curName = setcombo2$ContactName[i]
  if(!is.na(curName)){
    splitName = strsplit(curName, " ")[[1]]
    nameCount = length(splitName)
    if(nameCount == 2){
      setcombo2$firstName[i] = splitName[1]
      setcombo2$lastName[i] = splitName[2]
    } else {
      setcombo2$FixName[i] = "Fix This"
    }
  }
}


setcombo2 = setcombo2[order(setcombo2$identifier),]

setcombo2 = DFna.to.empty(setcombo2)

# Export the file, fix crazy names and phone extensions, then reimport

write.csv(x = setcombo2, file = paste0(OutFolder, "initial contacts output.csv"), row.names = F)

imp.Contact = read.csv(file = paste0(OutFolder, "initial contacts output.csv"), stringsAsFactors = F)

colnames(imp.Contact) = c("studentNumber", "ContactName", "emailAddress", "relationshipType", "street", "city", "postalCode", 
                          "gender", "contactLivesWith", "addressPriorityOrder", "contactHasCustody", 
                          "contactAllowSchoolPickup", "contactReceivesMailings", "identifier", 
                          "isActive", "phoneNumberAsEntered", "phoneType", "phoneNumberPriorityOrder", "isPreferred", 
                          "isSMS", "extension", "prefix", "emailType", "isPrimaryEmailAddress", "isEmergencyContact", 
                          "firstName",	"middleName",	"lastName",	"suffix", "FixName")


allColNames = c("identifier",	"prefix",	"firstName",	"middleName",	"lastName",	"suffix",	"gender",	"employer",	"stateContactNumber",	"isActive",
                "emailAddress",	"emailType",	"isPrimaryEmailAddress",
                "phoneNumberAsEntered",	"extension",	"phoneType",	"phoneNumberPriorityOrder",	"isSMS",	"isPreferred",
                "street", "linetwo", "unit", "city", "state", "postalCode", "geocodeLatitude", "geocodeLongitude",
                "addressType", "addressPriorityOrder", "addressStartDate", "addressEndDate",
                "studentNumber",
                "originalContactType",	"relationshipType",	"relationshipNote",	"relationshipStartDate",	"relationshipEndDate",
                "contactHasCustody",	"contactLivesWith",	"contactAllowSchoolPickup",	"isEmergencyContact",	"contactReceivesMailings")

imp.Contact$employer = ""
imp.Contact$stateContactNumber = ""
imp.Contact$linetwo = ""
imp.Contact$unit = ""
imp.Contact$state = ""
imp.Contact$state[!is.na(imp.Contact$street)] = "NY"
imp.Contact$state[imp.Contact$street == ""] = ""
imp.Contact$geocodeLatitude = ""
imp.Contact$geocodeLongitude = ""
imp.Contact$addressStartDate = ""
imp.Contact$addressEndDate = ""
imp.Contact$originalContactType = imp.Contact$relationshipType
imp.Contact$relationshipNote = ""
imp.Contact$relationshipStartDate = ""
imp.Contact$relationshipEndDate = ""

imp.Contact$addressType = ""
imp.Contact$addressType[!is.na(imp.Contact$street)] = "Home"
imp.Contact$addressType[imp.Contact$street == ""] = ""

setdiff(allColNames, colnames(imp.Contact))
setdiff(colnames(imp.Contact),allColNames)

imp.Contact = imp.Contact[,allColNames]

imp.Contact = DFna.to.empty(imp.Contact)

write.psimport(x = imp.Contact[c(1,4:nrow(imp.Contact)),], file = paste0(OutFolder, "ContactsImport.txt"))
write.psimport(x = imp.Contact, file = paste0(OutFolder, "ContactsImport.txt"))


write.table(x = imp.Contact, file = paste0(OutFolder, "ContactsImport.txt"), quote = F, sep = "\t", eol = "\n", na = "", row.names = F)
