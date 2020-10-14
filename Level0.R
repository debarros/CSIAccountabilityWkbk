# Level0.R

#--------------------#
#### Demographics ####
#--------------------#

# Get the advisory portion of the cc table (for students missing race and ethnicity)
cc.adv = cc.raw[grepl("Advisory", cc.raw$`[02]course_name`, T),]
cc.adv = cc.adv[order(cc.adv$DateEnrolled, decreasing = T),]
cc.adv = cc.adv[!duplicated(cc.adv$`[01]Student_Number`),]


# Load the demographics file produced by PowerSchool
demographics = read.csv(file.choose(), header = F, stringsAsFactors = F)


# Use the nysed template to assign the column names to the demographics file
x = dBtools::GetNiceColumnNames("STUDENT LITE", templates)
colnames(demographics) = x[1:ncol(demographics)]

# reformat the demographics file
demographics = DFna.to.empty(demographics)              # make blank spaces blank (not NA)

gradelevels = demographics$CURRENTGRADELEVELGRADELEVEL  # grab grade levels
gradelevels = as.character(gradelevels)                 # make Grade Level character to preserve 0's
gradelevels[nchar(gradelevels) == 1] = paste0("0",gradelevels[nchar(gradelevels) == 1])             # left pad single digit grade levels
demographics$CURRENTGRADELEVELGRADELEVEL = gradelevels  # put grade levels back in

demographics$GUIDANCECOUNSELORDISTRICTCODE = ""         # remove the guidance counselor codes
demographics$GUIDANCECOUNSELORID = ""

dips = demographics$DIPLOMATYPECODECREDENTIALTYPECODE   # grab the diploma types
dips[dips == "68"] = "068"                              # change diploma type code 68 to 068
dips[dips == "85"] = "085"                              # change diploma type code 85 to 085
demographics$DIPLOMATYPECODECREDENTIALTYPECODE = dips   # put the diploma type codes back in


# Deal with missing races
races = demographics$ETHNICCODESHORTRACE1CODE           # grab races
if(any(races == "")){                                   # Check for students missing race/ethnicity
  missRace = demographics[races == "",4:8]
  missRace$Advisor = cc.adv$`[05]lastfirst`[match(missRace$STUDENTIDSCHOOLDISTRICTSTUDENTID, cc.adv$`[01]Student_Number`)]
  missRace = missRace[order(missRace$Advisor, missRace$LASTNAMESHORTSTUDENTSLASTNAME),]
  write.csv(missRace, paste0(OutFolder,"missing student race and ethnicity.csv"))
  print("There are students missing race and ethnicity.  Check the csv file.")
  races[races == ""] = "B"                        # change missing race to Black
  demographics$ETHNICCODESHORTRACE1CODE = races   # put races back in
} else {
  print("No students are missing race and ethnicity information.")
} # /if=else there are blank races
  
write.SIRS(demographics, paste0(OutFolder, "demographics.csv"))    # output

str(demographics)

View(demographics[demographics$RACE2CODE == "N",])

#------------------#
#### Enrollment ####
#------------------#

enrollment = read.csv(file.choose(), stringsAsFactors = F)
colnames(enrollment) = GetNiceColumnNames("SCHOOL ENTRY EXIT", templates)
enrollment$SCHOOLENTRYTYPECODEREASONFORBEGINNINGENROLLMENTCODE = "0011"
toFix = enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE == "2021-06-29" & is.na(enrollment$SCHOOLEXITTYPECODEREASONFORENDINGENROLLMENTCODE)
enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE[toFix] = ""
enrollment = DFna.to.empty(enrollment)
gradelevels = enrollment$ENROLLMENTGRADELEVELGRADELEVEL  # grab grade levels
gradelevels = as.character(gradelevels)                 # make Grade Level character to preserve 0's
gradelevels[nchar(gradelevels) == 1] = paste0("0",gradelevels[nchar(gradelevels) == 1])             # left pad single digit grade levels
enrollment$ENROLLMENTGRADELEVELGRADELEVEL = gradelevels  # put grade levels back in
write.SIRS(enrollment, paste0(OutFolder, "enrollment.csv"))


graduates = demographics$STUDENTIDSCHOOLDISTRICTSTUDENTID[demographics$DIPLOMATYPECODECREDENTIALTYPECODE != ""]
oldEnroll = read.csv(file.choose(), stringsAsFactors = F)

colnames(oldEnroll) = GetNiceColumnNames("SCHOOL ENTRY EXIT", templates)
str(enrollment)
View(enrollment[enrollment$STUDENTIDSCHOOLDISTRICTSTUDENTID %in% graduates,])
enrollment$SCHOOLENTRYTYPECODEREASONFORBEGINNINGENROLLMENTCODE = "0011"
toFix = which(enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE == "" & enrollment$STUDENTIDSCHOOLDISTRICTSTUDENTID %in% graduates)
enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE[toFix] = "2020-06-29"
enrollment$SCHOOLEXITTYPECODEREASONFORENDINGENROLLMENTCODE[toFix] = "799"

gradelevels = enrollment$ENROLLMENTGRADELEVELGRADELEVEL  # grab grade levels
gradelevels = as.character(gradelevels)                 # make Grade Level character to preserve 0's
gradelevels[nchar(gradelevels) == 1] = paste0("0",gradelevels[nchar(gradelevels) == 1])             # left pad single digit grade levels
enrollment$ENROLLMENTGRADELEVELGRADELEVEL = gradelevels  # put grade levels back in

enrollment = DFna.to.empty(enrollment)

enrollment[enrollment$STUDENTIDSCHOOLDISTRICTSTUDENTID == "151610258",]
sort(enrollment$STUDENTIDSCHOOLDISTRICTSTUDENTID)

oldEnroll[oldEnroll$STUDENTIDSCHOOLDISTRICTSTUDENTID == 151610258,]

write.SIRS(enrollment, paste0(OutFolder, "enrollment.csv"))

#------------------------#
#### Program Services ####
#------------------------#

#All this does is fix lunch status eligibility codes from "NSLP APPLICATION" to "APPLICATION"

pfacts = read.csv(file.choose(), stringsAsFactors = F, header = F) # load the program facts export
colnames(pfacts) = GetNiceColumnNames("PROGRAMS FACT", templates)  # set the column names
toFix = nchar(pfacts$PROGRAMSCODEPROGRAMSERVICECODE) < 4           # find the codes that were truncated
Fixed = paste0("0", pfacts$PROGRAMSCODEPROGRAMSERVICECODE[toFix])  # make fixed versions of truncated codes
pfacts$PROGRAMSCODEPROGRAMSERVICECODE[toFix] = Fixed               # put fixed codes back in the data.frame
pfacts = DFna.to.empty(pfacts)                                     # replace NA's with blanks

if(any(pfacts == "NSLP APPLICATION")){
  print("Some students still have NSLP APPLICATION for their Program Eligibility Code.")
  for(i in 1:5){
    columnName = paste0("PROGRAMELIGIBILITYCODE", i)
    values = pfacts[,columnName]
    values[values == "NSLP APPLICATION"] = "APPLICATION"
    pfacts[,columnName] = values
  }
}

values = pfacts$STATELOCATIONIDPROGRAMSERVICEPROVIDERBEDSCODE  # grab the BEDS code
values[values == "10100860907"] = "010100860907"               # fix it
pfacts$STATELOCATIONIDPROGRAMSERVICEPROVIDERBEDSCODE = values  # put it back

write.SIRS(pfacts, file = "pfacts.csv") #output the fixed program facts file


#---------------------#
#### Diploma types ####
#---------------------#

# Note sure what this section does
demographics$DIPLOMATYPECODECREDENTIALTYPECODE
unique(demographics$DIPLOMATYPECODECREDENTIALTYPECODE)

demographics[which(demographics$DiplomaTypeCode == 762),]

demographics$STUDENTIDSCHOOLDISTRICTSTUDENTID[!is.na(demographics$DIPLOMATYPECODECREDENTIALTYPECODE)]



#----------------------------------#
#### Student Class Grade Detail ####
#----------------------------------#

# This section fixes the student class grade detail report by populating the credits attempted and credits earned

SCGD = read.csv(file.choose(), header = F, stringsAsFactors = F)            # read in the file
x = dBtools::GetNiceColumnNames("STUDENT CLASS GRADE DETAIL", templates)    # Add column names
colnames(SCGD) = x
SCGD$CREDITSATTEMPTED = 1                                                   # Default credits attempted to 1
SCGD$CREDITSATTEMPTED[SCGD$MARKINGPERIODCODE > 1] = .5                      # For semester courses, make it .5
for(i in 1:nrow(SCGD)){                                                     # For each entry
  if(SCGD$ALPHAGRADE[i] != "F"){                                            # If the student didn't fail
    SCGD$CREDITSEARNED[i] = SCGD$CREDITSATTEMPTED[i]                        # Set credits earned to same as credits attempted
  } # /if
} # /for

SCGD = DFna.to.empty(SCGD)                                                  # Convert NAs to empty strings
SCGD$SUPPLEMENTARYCOURSEDIFFERENTIATOR = NA                                 # This field has to be NA for some reason
SCGD$DUALCREDITCODE[SCGD$COURSECODELOCALCOURSECODE == "AP 1113H"] = "INDST" # Add the "in district" code for dual credit classes

for(i in 1:nrow(SCGD)){
  if(SCGD$COURSECODELOCALCOURSECODE[i] == "AP 1113H"){
    SCGD$POSTSECONDARYCREDITUNITS[i] = sum(college.raw$Credits[college.raw$ID == SCGD$STUDENTIDSCHOOLDISTRICTSTUDENTID[i]])
    if(is.na(SCGD$POSTSECONDARYCREDITUNITS[i])){
      SCGD$POSTSECONDARYCREDITUNITS[i] = 0
    }
  }
}

summary(as.factor(SCGD$POSTSECONDARYCREDITUNITS), )

write.SIRS(SCGD, file = paste0(OutFolder,"SCGD.csv"))                       # Output the file




#------------------------------------#
#### Course Instructor Assignment ####
#------------------------------------#


CIA = read.csv(file.choose(), stringsAsFactors = F)
colnames(CIA) = GetNiceColumnNames("COURSE INSTRUCTOR ASSIGNMENT", templates)



str(CIA)
