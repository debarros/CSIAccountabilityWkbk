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
demographics[which(is.na(demographics), arr.ind = T)] = ""                                # make blank spaces blank (not NA)
demographics[,8] = as.character(demographics[,8])                                         # make Grade Level character to preserve 0's
demographics[nchar(demographics[,8]) == 1,8] = "09"                                       # change single-digit grade levels to "09"
demographics[demographics[,24] == "68",24] = "068"                                        # change diploma type code 68 to 068

if(any(demographics$ETHNICCODESHORTRACE1CODE == "")){                                     # Check for students missing race/ethnicity
  write.csv(demographics[demographics$ETHNICCODESHORTRACE1CODE == "",4:8], 
            "missing student race and ethnicity.csv")
  print("There are students missing race and ethnicity.  Check the csv file.")
}

demographics$ETHNICCODESHORTRACE1CODE[demographics$ETHNICCODESHORTRACE1CODE == ""] = "B"  # change missing race to Black
write.table(demographics, "demographics.csv", row.names = F,                              # output 
            col.names = F, sep = ",", dec = ".") 





#------------------#
#### Enrollment ####
#------------------#


#------------------------#
#### Program Services ####
#------------------------#

#All this does is fix lunch status eligibility codes from "NSLP APPLICATION" to "APPLICATION"

pfacts = read.csv(file.choose(), stringsAsFactors = F, header = F)
x = GetNiceColumnNames("PROGRAMS FACT", templates)
colnames(pfacts) = x
for(i in 1:nrow(pfacts)){
  if(nchar(pfacts$PROGRAMSCODEPROGRAMSERVICECODE[i]) < 4){
    pfacts$PROGRAMSCODEPROGRAMSERVICECODE[i] = paste0("0", pfacts$PROGRAMSCODEPROGRAMSERVICECODE[i])
  }
}


for(i in 1:ncol(pfacts)){
  pfacts[,i] = na.to.empty(pfacts[,i])
}


for(i in 1:5){
  columnName = paste0("PROGRAMELIGIBILITYCODE", i)
  values = pfacts[,columnName]
  values[values == "NSLP APPLICATION"] = "APPLICATION"
  pfacts[,columnName] = values
}

values = pfacts$STATELOCATIONIDPROGRAMSERVICEPROVIDERBEDSCODE
values[values == "10100860907"] = "010100860907"
pfacts$STATELOCATIONIDPROGRAMSERVICEPROVIDERBEDSCODE = values


write.table(pfacts, file = "pfacts.csv", row.names = F, col.names = F, sep = ",", dec = ".") #output the fixed program facts file



#---------------------#
#### Diploma types ####
#---------------------#

demographics$DiplomaTypeCode
unique(demographics$DiplomaTypeCode)

demographics[which(demographics$DiplomaTypeCode == 762),]

demographics$StudentId[!is.na(demographics$DiplomaTypeCode)]


