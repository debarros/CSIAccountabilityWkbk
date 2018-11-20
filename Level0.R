# Level0.R

#--------------------#
#### Demographics ####
#--------------------#

# Get the advisory portion of the cc table (for students missing race and ethnicity)
cc.adv = cc.raw[grepl("Advisory", cc.raw$`[02]course_name`, T),]

# Load the demographics file produced by PowerSchool
demographics = read.csv(file.choose(), header = F, stringsAsFactors = F)


# Use the nysed template to assign the column names to the demographics file
x = dBtools::GetNiceColumnNames("STUDENT LITE", templates)
colnames(demographics) = x[1:ncol(demographics)]

# reformat the demographics file
demographics = DFna.to.empty(demographics)              # make blank spaces blank (not NA)

gradelevels = demographics$CURRENTGRADELEVELGRADELEVEL  # grab grade levels
gradelevels = as.character(gradelevels)                 # make Grade Level character to preserve 0's
gradelevels[nchar(gradelevels) == 1] = "09"             # change single-digit grade levels to "09"
demographics$CURRENTGRADELEVELGRADELEVEL = gradelevels  # put grade levels back in

demographics$GUIDANCECOUNSELORDISTRICTCODE = ""         # remove the guidance counselor codes

dips = demographics$DIPLOMATYPECODECREDENTIALTYPECODE   # grab the diploma types
dips[dips == "68"] = "068"                              # change diploma type code 68 to 068
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



#------------------#
#### Enrollment ####
#------------------#


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

demographics$StudentId[!is.na(demographics$DiplomaTypeCode)]


