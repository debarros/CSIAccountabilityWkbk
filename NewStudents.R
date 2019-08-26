# NewStudents.R 

# This script does some stuff with the application tracker.
# The first part determines the school districts
# The second part creates the powerschool import files


####              NOTE!!!           ####
# Before doing an upload, compare new students to existing students to see if there are any repeat applicants.




#---------------------------------------------------#
#### Pull the data from the Application Trackers ####
#---------------------------------------------------#

# This will need to be updated every year with the URLs of the trackers.
HS.address = "https://docs.google.com/spreadsheets/d/14uv-akqxoghRuQFMEhOB0wmTffaxnGRs6Yo5GM6ZHYE/edit?usp=sharing"
MS.address = "https://docs.google.com/spreadsheets/d/192PEJp4ft4kukvLt6UgjX9Qs5rNrV1UQXaanrHtQvU0/edit?usp=sharing"

HS.sheet = SWSM(gs_url(HS.address, verbose = F))       
HS.appSheet = gs_read(ss = HS.sheet, ws = "Tracker", range = "A2:DC300", verbose = F)
HS.appSheet = HS.appSheet[!is.na(HS.appSheet$Status),]

MS.sheet = SWSM(gs_url(MS.address, verbose = F))      
MS.appSheet = gs_read(ss = MS.sheet, ws = "Tracker", range = "A2:DC300", verbose = F)
MS.appSheet = MS.appSheet[!is.na(MS.appSheet$Status),]


MS.appSheet$FullAddress = paste0(MS.appSheet$Address, ", ", MS.appSheet$City, ", ", "NY", " ", MS.appSheet$Zip)
MS.appSheet$FullAddress = gsub(pattern = "#", replacement = "", x = MS.appSheet$FullAddress, fixed = T)
MS.appSheet$FullAddress2 = paste0(MS.appSheet$`Address 2`, ", ", MS.appSheet$`City 2`, ", ", "NY", " ", MS.appSheet$`Zip 2`)
MS.appSheet$FullAddress2[is.na(MS.appSheet$`Address 2`)] = NA_character_
MS.appSheet$CensusDistrict = ""


HS.appSheet$FullAddress = paste0(HS.appSheet$Address, ", ", HS.appSheet$City, ", ", "NY", " ", HS.appSheet$Zip)
HS.appSheet$FullAddress = gsub(pattern = "#", replacement = "", x = HS.appSheet$FullAddress, fixed = T)
HS.appSheet$FullAddress2 = paste0(HS.appSheet$`Address 2`, ", ", HS.appSheet$`City 2`, ", ", "NY", " ", HS.appSheet$`Zip 2`)
HS.appSheet$FullAddress2[is.na(HS.appSheet$`Address 2`)] = NA_character_
HS.appSheet$CensusDistrict = ""


# This pulls from old trackers.  It will need to be updated every year with the URLs of last year's trackers.
url1819 = "https://docs.google.com/spreadsheets/d/18ida9wqcgxzyT8j5N2pfKsjYCIEBa7hwgxX_nn7HmK8/edit?usp=sharing"
url1718 = "https://docs.google.com/spreadsheets/d/1mLuUQIFArmhvUMCYG0JciOZbeckvindxCloSj_q93TE/edit?usp=sharing"
apsheet1819 = gs_read(ss = gs_url(x = url1819, verbose = F), ws = "Tracker", range = "A2:AA300", verbose = F)
apsheet1718 = gs_read(ss = gs_url(x = url1718, verbose = F), ws = "Tracker", range = "A2:AA300", verbose = F)



#-----------------------------------#
#### Check for Repeat Applicants ####
#-----------------------------------#

# Only run this section if you haven't uploaded students yet

firstNames = c(HS.appSheet$`Student's First Name`, MS.appSheet$`Student's First Name`)
lastNames = c(HS.appSheet$`Student's Last Name`, MS.appSheet$`Student's Last Name`)
fullNames = paste0(lastNames,", ",firstNames)
fullNames = gsub(pattern = "\\W", replacement = "", x = fullNames)
fullNames = toupper(fullNames)


existingFullNames = c(paste0(Workbook$Last.Name, ", ", Workbook$First.Name), 
                      powerschoolraw$lastfirst, 
                      paste0(apsheet1819$`Student's Last Name`, ", ", apsheet1819$`Student's First Name`),
                      paste0(apsheet1718$`Student's Last Name`, ", ", apsheet1718$`Student's First Name`))
existingFullNames = gsub(pattern = "\\W", replacement = "", x = existingFullNames)
existingFullNames = toupper(existingFullNames)
existingFullNames = existingFullNames[!duplicated(existingFullNames)]

# For each applicant whose name is substantially similar to a prior student or applicant,
#     Print the new applicant's name, and then print the matching names.
for(i in 1:length(fullNames)){
  thisMatches = agrep(pattern = fullNames[i], x = existingFullNames, value = T)
  if(length(thisMatches) > 0){
    print(fullNames[i])
    print(paste0(thisMatches, collapse = ", "))
    print("")
  }
}




#-------------------------------------#
#### Check for phone number errors ####
#-------------------------------------#




phoneVars = c(paste0("Number", " ", apply(X = expand.grid(1:2, 1:3), MARGIN = 1, FUN = paste0, collapse = ".")))

# Check HS first
phoneNums = unlist(c(HS.appSheet[,phoneVars]))
phoneNums = phoneNums[!is.na(phoneNums)]
badNumbers = grep(pattern = "^[[:digit:]]{3}-[[:digit:]]{3}-[[:digit:]]{4}$", x = phoneNums, value = T, invert = T)  
if(length(badNumbers) > 0){
  print("There are bad phone numbers in the HS tracker!")
  print(badNumbers)
} else {
  print("Phone numbers in the HS tracker look good.")
}


# Check MS next
phoneNums = unlist(c(MS.appSheet[,phoneVars]))
phoneNums = phoneNums[!is.na(phoneNums)]
badNumbers = grep(pattern = "^[[:digit:]]{3}-[[:digit:]]{3}-[[:digit:]]{4}$", x = phoneNums, value = T, invert = T)  
if(length(badNumbers) > 0){
  print("There are bad phone numbers in the MS tracker!")
  print(badNumbers)
} else {
  print("Phone numbers in the MS tracker look good.")
}

# which(MS.appSheet == badNumbers, arr.ind = T)
# MS.appSheet[63,26] = ""

#--------------------------------------------------------#
#### Query the Census API to get the school districts ####
#--------------------------------------------------------#

for(i in 1:nrow(HS.appSheet)){
  print(paste0(i, " of ", nrow(HS.appSheet)))
  HS.appSheet$CensusDistrict[i] = getSchoolDistrict(addr = HS.appSheet$FullAddress[i], halt = F)
}

for(i in 1:nrow(MS.appSheet)){
  print(paste0(i, " of ", nrow(MS.appSheet)))
  MS.appSheet$CensusDistrict[i] = getSchoolDistrict(addr = MS.appSheet$FullAddress[i], halt = F)
}

# Test an address using the next line
# getSchoolDistrict("10 W. Sunnyside Street, Troy, NY 12180", halt = F)


#----------------------------------------------------------#
#### Examine the results, comparing to the entered DORs ####
#----------------------------------------------------------#

HS.appSheet$CenDistShort = substr(HS.appSheet$CensusDistrict, 1, 20)
table(as.data.frame(HS.appSheet[,c("CenDistShort", "DOR Name")]), useNA = "ifany")


MS.appSheet$CenDistShort = substr(MS.appSheet$CensusDistrict, 1, 20)
table(as.data.frame(MS.appSheet[,c("CenDistShort", "DOR Name")]), useNA = "ifany")

#----------------------------------------------------------#
#### Generate output for the addresses that didn't work ####
#----------------------------------------------------------#

DORlookup = SWSM(gs_read(ss = HS.sheet, ws = "Lists", range = "C2:F32", verbose = F))

HS.appSheet$DOR2 = DORlookup$`Census Name`[match(HS.appSheet$`DOR Name`, DORlookup$Name)]
appSheet.limited = HS.appSheet[!VbetterComp(HS.appSheet$DOR2, HS.appSheet$CensusDistrict), 
                               c("Student ID", "FullAddress", "FullAddress2", "CensusDistrict", "DOR2")]
colnames(appSheet.limited) = c("Student ID", "FullAddress", "FullAddress2", "AccordingToCensusUsingAddress", "AccordingToTracker")
write.csv(x = appSheet.limited, file = paste0(OutFolder,"HS school district errors.csv"))

MS.appSheet$DOR2 = DORlookup$`Census Name`[match(MS.appSheet$`DOR Name`, DORlookup$Name)]
appSheet.limited = MS.appSheet[!VbetterComp(MS.appSheet$DOR2, MS.appSheet$CensusDistrict), 
                               c("Student ID", "FullAddress", "FullAddress2", "CensusDistrict", "DOR2")]
colnames(appSheet.limited) = c("Student ID", "FullAddress", "FullAddress2", "AccordingToCensusUsingAddress", "AccordingToTracker")
write.csv(x = appSheet.limited, file = paste0(OutFolder,"MS school district errors.csv"))

# If the AccordingToCensusUsingAddress field is NA, the address might be bad, or it might be a housing complex
# We should make a list of housing complexes and their associated school districts

# If the AccordingToTracker field is NA, either the DOR is not entered for that student, 
# or it doesn't have a Census Name in the Districts table on the Lists tab



#-----------------------------------------------------------------#
#### Make a data.frame of students to import and add race info ####
#-----------------------------------------------------------------#

statuses2use = c("Applying", "Applied", "Conditionally accepted", "Accepted", "Enrolled", "Wait Listed", "Conditionally Wait Listed")

keepCols = intersect(colnames(HS.appSheet), colnames(MS.appSheet))

combined.appSheet = rbind(HS.appSheet[,keepCols], MS.appSheet[,keepCols])
unique(combined.appSheet$Status)

importable = as.data.frame(combined.appSheet[combined.appSheet$Status %in% statuses2use,])
rownames(importable) = NULL
importable$RepeaterFlag = toupper(importable$`Repeater?`) == "YES"

importable$racecount = 0
for(i in 1:nrow(importable)){
  importable$racecount[i] = 5 - sum(is.na(importable[i,c("Black?", "White?", "Asian?", "Native American?", "Pacific Islander?")]))
}

importable$Ethnicity = ""
importable$Ethnicity[VbetterComp(importable$`Black?`, "Y")] = "B"
importable$Ethnicity[VbetterComp(importable$`White?`, "Y")] = "W"
importable$Ethnicity[VbetterComp(importable$`Pacific Islander?`, "Y")] = "P"
importable$Ethnicity[VbetterComp(importable$`Asian?`, "Y")] = "A"
importable$Ethnicity[VbetterComp(importable$`Native American?`, "Y")] = "N"
importable$Ethnicity[importable$racecount > 1] = "M"
importable$Ethnicity[VbetterComp(importable$`Hispanic?`, "Y")] = "H"

#---------------------------------------------------------------#
#### Make the flat file to import into the PS Students table ####
#---------------------------------------------------------------#

imp.Stu = data.frame(Student_Number = importable$`Student ID`)

suffixes = importable$Suffix
suffixes[!is.na(suffixes)] = paste0(" ", suffixes[!is.na(suffixes)])
suffixes[is.na(suffixes)] = ""
imp.Stu$Last_Name = paste0(importable$`Student's Last Name`, suffixes)

imp.Stu$First_Name = importable$`Student's First Name`

midnames = importable$`Student's Middle Name`
midnames[is.na(midnames)] = ""
imp.Stu$Middle_Name = midnames

DOBs = importable$`Date of Birth`
DOBs = gsub(pattern = "^([[:digit:]]/)", replacement = "0\\1", x = DOBs)
DOBs = gsub(pattern = "(/)([[:digit:]]/)", replacement = "/0\\2", x = DOBs)
imp.Stu$DOB = DOBs

if(any(is.na(imp.Stu$DOB))){
  print("Uh oh!  There are students with missing or bad DOBs.")
  print(imp.Stu[is.na(imp.Stu$DOB), c("Student_Number", "Last_Name", "First_Name")])
}


imp.Stu$Grade_Level = importable$`Grade Applying For`  

# imp.Stu$Home_Phone = importable$`Home 1`

imp.Stu$Street = importable$Address
imp.Stu$City = importable$City
imp.Stu$Zip = importable$Zip
imp.Stu$State = "NY"

imp.Stu$DistrictOfResidence = importable$`DOR Code`

hispanic = importable$`Hispanic?`
hispanic[hispanic == "Y"] = "1"
hispanic[is.na(hispanic)] = "0"
imp.Stu$FedEthnicity = hispanic

imp.Stu$Gender = "M"

imp.Stu$Enroll_Status = "0"
imp.Stu$EntryCode = "0011"
imp.Stu$EntryDate = "08/20/2019"                                  # Update each year
imp.Stu$ExitDate = "06/29/2020"                                   # Update each year
imp.Stu$TransferComment = paste0("Enrolled Fall ", schoolYear())
imp.Stu$SchoolEntryDate = "08/20/2019"                            # Update each year

imp.Stu$SchoolID = "100860907"
imp.Stu$Enrollment_SchoolID = "100860907"
imp.Stu$Next_School = "100860907"

imp.Stu$Sched_Scheduled = "1"

imp.Stu$Sched_YearOfGraduation = schoolYear() + 13 - imp.Stu$Grade_Level

imp.Stu$Sched_NextYearGrade = imp.Stu$Grade_Level + 1

imp.Stu$Ethnicity = importable$Ethnicity

imp.Stu$SchoolEntryGradeLevel = imp.Stu$Grade_Level
imp.Stu$Track = "A"

imp.Stu$Mailing_City = imp.Stu$City
imp.Stu$Mailing_State = imp.Stu$State
imp.Stu$Mailing_Street = imp.Stu$Street
imp.Stu$Mailing_Zip = imp.Stu$Zip

imp.Stu$Lunch_ID = substr(imp.Stu$Student_Number, 5, 9)

imp.Stu$S_NY_STU_X.DateOfEntryGrade9 = "08/20/2019"                 # Update each year
imp.Stu$S_NY_STU_X.DateOfEntryGrade9[imp.Stu$Grade_Level != 9] = ""
imp.Stu$S_NY_STU_X.DateOfEntryGrade9[imp.Stu$RepeaterFlag] = ""

imp.Stu$FTEID = 502                                                 # Update each yeach

write.psimport(x = imp.Stu, file = paste0(OutFolder, "StudentImport.txt"))


#------------------------------------------------------------------#
#### Make the flat file to import into the PS StudentRace table ####
#------------------------------------------------------------------#

# Note: The Students tab in the PowerSchool excel workbook must already have the new students in it for this to work
# This means that the students import will have to have happened already.

raceColumns = c("Black?", "White?", "Asian?", "Native American?", "Pacific Islander?")
imp.Race = importable[,c("Student ID", raceColumns)]
imp.Race$StudentID = powerschoolraw$ID[match(imp.Race$`Student ID`, powerschoolraw$student_number)]

if(nrow(imp.Race[is.na(imp.Race$StudentID),]) > 0){
  print("Warning!  There are importable students in the tracker who are not in PowerSchool!")
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

stu.importable = importable
# stu.importable$StudentID = powerschoolraw$ID[match(stu.importable$`Student ID`, powerschoolraw$student_number)]


if(nrow(stu.importable[is.na(stu.importable$StudentID),]) > 0){
  print("Warning!  There are stu.importable students in the tracker who are not in PowerSchool!")
  print("")
  print(stu.importable[is.na(stu.importable$StudentID),])
}

stu.importable = stu.importable[!is.na(stu.importable$StudentID),] # remove rows for students not in PowerSchool

stu.importable$contactLivesWith1 = "1"
stu.importable$contactLivesWith2 = toupper(stu.importable$`Lives With?`) %in% c("Y", "YES")

stu.importable$addressPriorityOrder1 = "1"
stu.importable$addressPriorityOrder2 = NA_character_
stu.importable$addressPriorityOrder2[!is.na(stu.importable$`Address 2`)] = "1"
stu.importable$addressPriorityOrder2[stu.importable$`Address 2` == ""] = ""


# Determine full set of unique Contact Persons

set1Vars = c("StudentID",
             "Prefix 1", "First name 1", "Middle name 1", "Last name 1",
             "Gender 1",
             "Email address 1",
             "Relationship 1",
             "Number 1.1", "Ext 1.1", "Type 1.1", "Prefer 1.1", "Texts 1.1",
             "Number 1.2", "Ext 1.2", "Type 1.2", "Prefer 1.2", "Texts 1.2",
             "Number 1.3", "Ext 1.3", "Type 1.3", "Prefer 1.3", "Texts 1.3",
             "Address", "City", "Zip", "addressPriorityOrder1",
             "contactLivesWith1")

set2Vars = c("StudentID",
             "Prefix 2", "First name 2", "Middle name 2", "Last name 2",
             "Gender 2",
             "Email address 2", 
             "Relationship 2",
             "Number 2.1", "Ext 2.1", "Type 2.1", "Prefer 2.1", "Texts 2.1",
             "Number 2.2", "Ext 2.2", "Type 2.2", "Prefer 2.2", "Texts 2.2",
             "Number 2.3", "Ext 2.3", "Type 2.3", "Prefer 2.3", "Texts 2.3",
             "Address 2", "City 2", "Zip 2", "addressPriorityOrder2",
             "contactLivesWith2",
             "Mail to address 2 also?",
             "Parent 2 also has custody?",
             "Parent 2 can pick up student from school?")


set1 = stu.importable[,c(set1Vars)] # 1st parent/guardian
set2 = stu.importable[,c(set2Vars)] # 2nd parent/guardian

set1$getmail = 1
set1$hascustody = 1
set1$canpickup = 1

set2$`Mail to address 2 also?`[set2$`Mail to address 2 also?` == "No"] = NA
set2$`Mail to address 2 also?`[set2$`Mail to address 2 also?` == "Yes"] = "1"


p1Cols = c("p1Num", "p1Ext", "p1Type", "p1Pref", "p1Text")
p2Cols = c("p2Num", "p2Ext", "p2Type", "p2Pref", "p2Text")
p3Cols = c("p3Num", "p3Ext", "p3Type", "p3Pref", "p3Text")

newColNames = c("StudentID", 
                "Prefix", "First", "Middle", "Last", 
                "Gender",
                "Email", 
                "Relationship",
                p1Cols, p2Cols, p3Cols,
                "Address", "City", "Zip", "addressPriorityOrder",
                "contactLivesWith", 
                "getmail",
                "hascustody", "canpickup")

colnames(set1) = newColNames
colnames(set2) = newColNames




# The next lines remove blank rows from set2 (the second parent/guardian)
set2$Drop = NA
for(i in 1:nrow(set2)){
  set2$Drop[i] = all(is.na(set2[i,c("Prefix", "First", "Middle", "Last")]))
}
set2 = set2[!set2$Drop,]
set2$Drop = NULL


set2$hascustody[set2$hascustody == "No"] = NA
set2$hascustody[set2$hascustody == "Yes"] = "1"
set2$canpickup[set2$canpickup == "No"] = NA
set2$canpickup[set2$canpickup == "Yes"] = "1"


setcombo = rbind(set1, set2) # Combine 1st and 2nd parent/guardian stuff

length(unique(ContactName))

# The next line checks for duplicate contacts
ContactName = paste0(setcombo$First," ",setcombo$Last)
if(sum(duplicated(ContactName)) > 0){
  print(paste0("There are duplicate parents.  You will have to deal with that.  Here are the names:"))
  print(ContactName[duplicated(ContactName)])
  write.csv(x = ContactName[duplicated(ContactName)], file = "Duplicate Contacts to Merge.csv")
  print("They are also in a file.  Merge them after the import.")
} else {
  print("Yay! No duplicate contact names.")
}

setcombo$identifier = 1:nrow(setcombo)
setcombo$isActive = 1

row1 = setcombo[, !(colnames(setcombo) %in% c(p2Cols, p3Cols))]
row2 = setcombo[, !(colnames(setcombo) %in% c(p1Cols, p3Cols))]
row3 = setcombo[, !(colnames(setcombo) %in% c(p1Cols, p2Cols))]

colnames(row2) = colnames(row1)
colnames(row3) = colnames(row1)


# There are some elements that don't get repeated when a particular contact has multiple lines
dropvars = c("StudentID", "Prefix", "First", "Middle", "Last", "Gender", 
             "Email", "Relationship", "Address", "City", "Zip", "addressPriorityOrder", "contactLivesWith", 
             "getmail", "hascustody", "canpickup", "isActive")
row2[,dropvars] = NA
row3[,dropvars] = NA

# Only include extra rows for a contact when the contact has multiple phone numbers
row2 = row2[!is.na(row2$p1Num),]
row3 = row3[!is.na(row3$p1Num),]

setcombo2 = rbind(row1, row2, row3)

# This next section used to be needed when the phone numbers were not organized correctly in the tracker
# phonecols = apply(X = expand.grid("phone", c("num", "typ", "NumberPriorityOrder"), 1:3), MARGIN = 1, FUN = paste0, collapse = "")
# phonecols = c(phonecols, apply(X = expand.grid(c("isPreferred", "isSMS"), 1:3), MARGIN = 1, FUN = paste0, collapse = ""))
# 
# setcombo[,phonecols] = NA_character_
# 
# 
# for(i in 1:nrow(setcombo)){
#   numbersfound = 0
#   numbers = setcombo[i,c("Home", "Cell", "Work")]
#   numbers = as.data.frame(t(numbers), stringsAsFactors = F)
#   numbers$type = rownames(numbers)
#   numbers = numbers[!is.na(numbers[,1]),]
#   if(nrow(numbers) > 0){
#     numbers$phoneNumberPriorityOrder = 1:nrow(numbers)
#     numbers$isPreferred = 0
#     numbers$isPreferred[1] = 1
#     numbers$isSMS = 0
#     for(j in 1:nrow(numbers)){
#       setcombo[i,paste0("phonenum",j)] = numbers[j,1]
#       setcombo[i,paste0("phonetyp",j)] = numbers[j,2]
#       setcombo[i,paste0("phoneNumberPriorityOrder",j)] = numbers[j,3]
#       setcombo[i,paste0("isPreferred",j)] = numbers[j,4]
#       setcombo[i,paste0("isSMS",j)] = numbers[j,5]
#     }
#   }
# }
# 
# 
# setcombo[,c("Home", "Cell", "Work")] = NULL
# dropvars = c("Email", "Address", "City", "Zip", "Gender", "isActive", "ContactName")
# row1 = setcombo
# row2 = setcombo[!is.na(setcombo$phonenum2),]
# row3 = setcombo[!is.na(setcombo$phonenum3),]
# 
# row1$phonenumber = row1$phonenum1
# row1$phonetyp = row1$phonetyp1
# row1$phoneNumberPriorityOrder = row1$phoneNumberPriorityOrder1
# row1$isPreferred = row1$isPreferred1
# row1$isSMS = row1$isSMS1
# 
# row2$phonenumber = row2$phonenum2
# row2$phonetyp = row2$phonetyp2
# row2$phoneNumberPriorityOrder = row2$phoneNumberPriorityOrder2
# row2$isPreferred = row2$isPreferred2
# row2$isSMS = row2$isSMS2
# row2$addressPriorityOrder = ""
# row2[,dropvars] = NA
# 
# 
# row3$phonenumber = row3$phonenum3
# row3$phonetyp = row3$phonetyp3
# row3$phoneNumberPriorityOrder = row3$phoneNumberPriorityOrder3
# row3$isPreferred = row3$isPreferred3
# row3$isSMS = row3$isSMS3
# row3$addressPriorityOrder = ""
# row3[,dropvars] = NA
# 
# setcombo2 = rbind(row1, row2, row3)
# setcombo2[,phonecols] = NULL
# setcombo2$phonetyp[VbetterComp(setcombo2$phonetyp, "Cell")] = "Mobile"
# setcombo2$extension = NA_character_


# Check for duplicate phone numbers
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

# This section was for adding the name prefix, but now that gets entered in a separate field
# setcombo2$prefix = NA_character_
# setcombo2$prefix[setcombo2$Gender == "M"] = "Mr."
# setcombo2$prefix[setcombo2$Gender == "F"] = "Ms."

setcombo2$emailType[!is.na(setcombo2$Email)] = "Current"
setcombo2$isPrimaryEmailAddress[!is.na(setcombo2$Email)] = "1"
setcombo2$isEmergencyContact = 1
setcombo2$isEmergencyContact[setcombo2$studentNumber == ""] = ""

# This section was for splitting names, but now the names are already split to begin with
# setcombo2$firstName = NA_character_
# setcombo2$middleName = NA_character_
# setcombo2$lastName = NA_character_
# setcombo2$suffix = NA_character_
# setcombo2$FixName = ""
# 
# for(i in 1:nrow(setcombo2)){
#   curName = setcombo2$ContactName[i]
#   if(!is.na(curName)){
#     splitName = strsplit(curName, " ")[[1]]
#     nameCount = length(splitName)
#     if(nameCount == 2){
#       setcombo2$firstName[i] = splitName[1]
#       setcombo2$lastName[i] = splitName[2]
#     } else {
#       setcombo2$FixName[i] = "Fix This"
#     }
#   }
# }


setcombo2 = setcombo2[order(setcombo2$identifier),] # Group together rows that refer to the same contact

setcombo2 = DFna.to.empty(setcombo2) # Convert NA entries to blanks

# This section was necessary before we started entering names in the tracker correctly
# # Export the file, fix crazy names and phone extensions, then reimport
# write.csv(x = setcombo2, file = paste0(OutFolder, "initial contacts output.csv"), row.names = F)
# # Now go fix crazy names
# imp.Contact = read.csv(file = paste0(OutFolder, "initial contacts output.csv"), stringsAsFactors = F)

colnames(setcombo2) = c("studentNumber", "prefix", "firstName",	"middleName",	"lastName", "gender", 
                        "emailAddress", "relationshipType", "phoneNumberAsEntered", "extension","phoneType", "isPreferred", 
                        "isSMS", "street", "city", "postalCode", "addressPriorityOrder", "contactLivesWith",  
                        "contactReceivesMailings", "contactHasCustody", "contactAllowSchoolPickup",  "identifier", "isActive", "emailType",
                        "isPrimaryEmailAddress", "isEmergencyContact")


allColNames = c("identifier",	"prefix",	"firstName",	"middleName",	"lastName",	"suffix",	"gender",	"employer",	"stateContactNumber",	"isActive",
                "emailAddress",	"emailType",	"isPrimaryEmailAddress",
                "phoneNumberAsEntered",	"extension",	"phoneType",	"phoneNumberPriorityOrder",	"isSMS",	"isPreferred",
                "street", "linetwo", "unit", "city", "state", "postalCode", "geocodeLatitude", "geocodeLongitude",
                "addressType", "addressPriorityOrder", "addressStartDate", "addressEndDate",
                "studentNumber",
                "originalContactType",	"relationshipType",	"relationshipNote",	"relationshipStartDate",	"relationshipEndDate",
                "contactHasCustody",	"contactLivesWith",	"contactAllowSchoolPickup",	"isEmergencyContact",	"contactReceivesMailings")

imp.Contact = setcombo2

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
imp.Contact$originalContactType = ""
imp.Contact$relationshipNote = ""
imp.Contact$relationshipStartDate = ""
imp.Contact$relationshipEndDate = ""

imp.Contact$addressType = ""
imp.Contact$addressType[!is.na(imp.Contact$street)] = "Home"
imp.Contact$addressType[imp.Contact$street == ""] = ""

imp.Contact$suffix = ""

# Establish phone number priority
imp.Contact$phoneNumberPriorityOrder = NA_integer_
imp.Contact$phoneNumberPriorityOrder[imp.Contact$isPreferred == "Y"] = 1
contactIDs = unique(imp.Contact$identifier)
singletonIDs = Filter(function (elem) length(which(imp.Contact$identifier == elem)) <= 1, imp.Contact$identifier)
singletonIDsWithPhone = intersect(singletonIDs, imp.Contact$identifier[imp.Contact$phoneNumberAsEntered != ""])
imp.Contact$phoneNumberPriorityOrder[imp.Contact$identifier %in% singletonIDsWithPhone] = 1
summary(as.factor(imp.Contact$phoneNumberPriorityOrder))

# See if any contacts have 2 preferred numbers
for(i in contactIDs){
  thesePrefs = imp.Contact$phoneNumberPriorityOrder[imp.Contact$identifier == i]
  thesePrefs = thesePrefs[!is.na(thesePrefs)]
  if(any(duplicated(thesePrefs))){
    print(i)
  }
}

i = 263
imp.Contact[i,]
# Add in preferences for all other rows
for(i in 1:nrow(imp.Contact)){
  if(imp.Contact$phoneNumberAsEntered[i] != ""){
    if(is.na(imp.Contact$phoneNumberPriorityOrder[i])){
      existingPriorities = imp.Contact$phoneNumberPriorityOrder[imp.Contact$identifier == imp.Contact$identifier[i]]
      if(all(is.na(existingPriorities))){
        imp.Contact$phoneNumberPriorityOrder[i] = 1
      } else {
        imp.Contact$phoneNumberPriorityOrder[i] = betterMax(existingPriorities) + 1
      }
    }
  }
}
summary(as.factor(imp.Contact$phoneNumberPriorityOrder))

imp.Contact[imp.Contact$identifier == 127,]

i = 1
for(i in 1:3){
  theseContacts = imp.Contact$identifier[imp.Contact$phoneNumberPriorityOrder == i]
  theseContacts = theseContacts[!is.na(theseContacts)]
  print(theseContacts[duplicated(theseContacts)])
}

setdiff(allColNames, colnames(imp.Contact))
setdiff(colnames(imp.Contact),allColNames)

imp.Contact = imp.Contact[,allColNames]

imp.Contact = DFna.to.empty(imp.Contact)


# Fix the way that isSMS and isPrefered are formatted
imp.Contact$isSMS[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$isSMS == "Y"] = 1
imp.Contact$isSMS[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$isSMS == "Yes"] = 1
imp.Contact$isSMS[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$isSMS == ""] = 0
imp.Contact$isSMS[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$isSMS == "No"] = 0
imp.Contact$isSMS[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$isSMS == "N"] = 0
imp.Contact$isPreferred[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$isPreferred == "Yes"] = 1
imp.Contact$isPreferred[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$isPreferred == "Y"] = 1
imp.Contact$isPreferred[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$isPreferred == ""] = 0
imp.Contact$isPreferred[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$isPreferred == "No"] = 0
imp.Contact$isPreferred[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$isPreferred == "N"] = 0


# For any phone number that has no type, call it a Home number
imp.Contact$phoneType[imp.Contact$phoneNumberAsEntered != "" & imp.Contact$phoneType == ""] = "Home"

# Fix the way gender is formatted
unique(imp.Contact$gender)
imp.Contact$gender[imp.Contact$gender == "Male"] = "M"
imp.Contact$gender[imp.Contact$gender == "Female"] = "F"
imp.Contact$gender[imp.Contact$gender == "Nonbinary"] = ""

# Fix the way prefix is formatted
imp.Contact$prefix[imp.Contact$prefix == "Ms"] = "Ms."
imp.Contact$prefix[imp.Contact$prefix == "Mr"] = "Mr."
imp.Contact$prefix[imp.Contact$prefix == "Mrs"] = "Mrs."

# Use the actual student number instead of the ID field from the Students table
imp.Contact$studentNumber = powerschoolraw$student_number[match(imp.Contact$studentNumber, powerschoolraw$ID)]
imp.Contact$studentNumber[is.na(imp.Contact$studentNumber)] = ""

# Since you are going to use the import manager, set type to "manager"
write.psimport(x = imp.Contact, file = paste0(OutFolder, "ContactsImport.txt"), type = "manager")

