# Level0.R

#load the demographics file produced by PowerSchool
demographics = read.csv(file.choose() ,header = F, stringsAsFactors = F)

str(demographics)

#Use the nysed template to assign the column names to the demographics file
x = read.xlsx(TEMPLATES, sheet = "STUDENT LITE", startRow = 2)[,3]
x = sub(pattern = "\n",replacement = "",x = x)
colnames(demographics) = x[1:ncol(demographics)]

# put the 9th grade enrollment dates in the demographics file
demographics[,26] = Workbook$`Cohort.Year.(year.1st.entered.9th)`[match(demographics[,4], Workbook$`Local.ID.(optional)`)] 


# reformate the demographics file
demographics[which(is.na(demographics), arr.ind = T)] = "" #make blank spaces blank (not NA) in the demographics file



demographics[,8] = as.character(demographics[,8]) #make the Grade Level a character value to preserve the 0's

demographics[nchar(demographics[,8]) == 1,8] = "09" #if there are any single-digit grade levels, make them "09"

demographics[demographics[,24] == "68",24] = "068" #if there are any diploma type codes 68, make them 068

demographics$`*ETHNIC CODE SHORT(RACE 1 CODE)`[demographics$`*ETHNIC CODE SHORT(RACE 1 CODE)` == ""] = "B" #if there are any students missing race, make them Black

write.table(demographics, file = "demographics.csv", row.names = F, col.names = F, sep = ",", dec = ".") #output the fixed demographics file



#### Enrollment ####


#### Program Services ####
#All this does is add lunch status

pfacts = read.csv("J:/2016-2017/Level 0/Program Fact/8005977_ProgramFact_SFTP_201701041041.csv", stringsAsFactors = F, header = F)
str(pfacts)
lunches = read.csv("C:/Users/pauldeba/Documents/Everything/data drive/2015-2016/stuff to upload to L0/Program Facts/lunches.csv", stringsAsFactors = F)
colnames(lunches)[1] = "V4"
str(lunches)
lunches$V1 = pfacts$V1[1]
lunches$V2 = 1
lunches$V3 = pfacts$V3[1]
lunches = lunches[nchar(lunches$Status)>0,]
lunches$V5 = NA_integer_
for (i in 1:nrow(lunches)){
  if(lunches$Status[i] == "R"){
    lunches$V5[i] = 5806
  } else {
    lunches$V5[i] = 5817
  }
}

workbook$Date.First.Enrolled.at.GTH = as.Date(workbook$Date.First.Enrolled.at.GTH, format = "%m/%d/%Y")
lunches$V6 = NA_character_
for (i in 1:nrow(lunches)){
  x = workbook$Date.First.Enrolled.at.GTH[match(lunches$V4[i], workbook$Local.ID..optional.)]
  if(x < as.Date("2015-07-01")){
    x = as.Date("2015-07-01")
  }
  lunches$V6[i] = as.character(x)
}

str(lunches)
str(pfacts)
workbook$Date.left.GTH = as.Date(workbook$Date.left.GTH, format = "%m/%d/%Y")
lunches$V7 = NA_character_
for (i in 1:nrow(lunches)){
  x = workbook$Date.left.GTH[match(lunches$V4[i], workbook$Local.ID..optional.)]
  if(is.na(x)){
    lunches$V7 = ""
  } else {
    lunches$V7 = as.character(x)
  }
}

lunches = lunches[,c("V1","V2","V3","V4","V5","V6","V7")]
pfacts = pfacts[,1:7]
str(lunches)
str(pfacts)


str(workbook$Date.left.GTH[5])
as.character(workbook$Date.left.GTH[5])


pfacts$V5 = as.character(pfacts$V5)
pfacts$V4 = as.character(pfacts$V4)
nchar(pfacts$V4)
for (i in 1:nrow(pfacts)){
  if(nchar(pfacts$V5[i]) == 3){
    pfacts$V5[i] = paste0("0",pfacts$V5[i])
  }
  if(nchar(pfacts$V4[i]) == 8){
    pfacts$V4[i] = paste0("0",pfacts$V4[i])
  }
}

nchar(pfacts$V4)

exporter = rbind.data.frame(pfacts, lunches)
exporter[is.na(exporter)] = ""

write.table(exporter, file = "pfacts.csv", row.names = F, col.names = F, sep = ",", dec = ".") #output the fixed program facts file

unique(pfacts$V5)

pfacts2 = read.csv("pfacts.csv", header = F)

str(pfacts2)

pfacts2.lunch = pfacts2[pfacts2$V5 %in% c(5817,5806),]
pfacts2.lunch$V5 = "0198"

pfacts2.lunch$V4 = as.character(pfacts2.lunch$V4)
nchar(pfacts2.lunch$V4)
mean(nchar(pfacts2.lunch$V4))
for (i in 1:nrow(pfacts2.lunch)){
  if(nchar(pfacts2.lunch$V4[i]) < 9){
    pfacts2.lunch$V4[i] = paste0("0",pfacts2.lunch$V4[i])
  }
}

write.table(pfacts2.lunch, file = "pfacts2.lunch.csv", row.names = F, col.names = F, sep = ",", dec = ".") #output the poverty program facts file

pfacts2.lunch$V7 = ""


#################
# Diploma types
demographics$DiplomaTypeCode
unique(demographics$DiplomaTypeCode)

demographics[which(demographics$DiplomaTypeCode == 762),]

demographics$StudentId[!is.na(demographics$DiplomaTypeCode)]


