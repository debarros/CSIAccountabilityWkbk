# Level0.R

#--------------------#
#### Demographics ####
#--------------------#

#load the demographics file produced by PowerSchool
demographics = read.csv(file.choose() ,header = F, stringsAsFactors = F)


#Use the nysed template to assign the column names to the demographics file
x = dBtools::GetNiceColumnNames("STUDENT LITE", templates)
colnames(demographics) = x[1:ncol(demographics)]

# reformat the demographics file
demographics = DFna.to.empty(demographics)              # make blank spaces blank (not NA)
gradelevels = demographics$CURRENTGRADELEVELGRADELEVEL  # grab grade levels
gradelevels = as.character(gradelevels)                 # make Grade Level character to preserve 0's
gradelevels[nchar(gradelevels) == 1] = "09"             # change single-digit grade levels to "09"
demographics$CURRENTGRADELEVELGRADELEVEL = gradelevels  # put grade levels back in
demographics[demographics[,24] == "68",24] = "068"      # change diploma type code 68 to 068

races = demographics$ETHNICCODESHORTRACE1CODE           # grab races
if(any(races == "")){                                   # Check for students missing race/ethnicity
  write.csv(demographics[races == "",4:8], 
            "missing student race and ethnicity.csv")
  print("There are students missing race and ethnicity.  Check the csv file.")
} # /if there are blank races
races[races == ""] = "B"                        # change missing race to Black
demographics$ETHNICCODESHORTRACE1CODE = races   # put races back in

demographics$GUIDANCECOUNSELORDISTRICTCODE = "" # remove the guidance counselor codes

write.SIRS(demographics, "demographics.csv")    # output



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

demographics$DIPLOMATYPECODECREDENTIALTYPECODE
unique(demographics$DIPLOMATYPECODECREDENTIALTYPECODE)

demographics[which(demographics$DiplomaTypeCode == 762),]

demographics$StudentId[!is.na(demographics$DiplomaTypeCode)]


