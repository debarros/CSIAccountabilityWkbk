# DemographicChanges.R

demofolder = "\\\\stuthin2/data/2017-2018/Level 0/student lite (demographics)"
demog.combo = read.csv.multi(folder = demofolder, header = F, idcol = "version")
demog.combo = as.data.frame(demog.combo)
colnames(demog.combo) = c("version",GetNiceColumnNames("STUDENT LITE", templates))


addrFields = c("ADDRESS1STUDENTSADDRESSLINE1", "CITYSTUDENTSADDRESSCITY", "ZIPCODESTUDENTSADDRESSZIPCODE", "DISTRICTCODEOFRESIDENCE")
demog.combo$Address = apply(X = demog.combo[,addrFields], MARGIN = 1, FUN = paste0, collapse = ", ")
students = data.frame(StudentID = unique(demog.combo$STUDENTIDSCHOOLDISTRICTSTUDENTID))
students$LastName = demog.combo$LASTNAMESHORTSTUDENTSLASTNAME[match(students$StudentID, demog.combo$STUDENTIDSCHOOLDISTRICTSTUDENTID)]
students$FirstName = demog.combo$FIRSTNAMESHORTSTUDENTSFIRSTNAME[match(students$StudentID, demog.combo$STUDENTIDSCHOOLDISTRICTSTUDENTID)]
students$StreetChange = F 
students$MuniChange = F 
students$ZipChange = F 
students$DORChange = F
studentIssues = demog.combo[0,]
emptyRow = demog.combo[1,]
for(i in 1:ncol(emptyRow)){
  emptyRow[1,i] = ""
}
for(i in 1:nrow(students)){
  currentSet = demog.combo[demog.combo$STUDENTIDSCHOOLDISTRICTSTUDENTID == students$StudentID[i],]
  if(length(unique(currentSet$ADDRESS1STUDENTSADDRESSLINE1))>1)  students$StreetChange[i] = T
  if(length(unique(currentSet$CITYSTUDENTSADDRESSCITY))>1)       students$MuniChange[i] = T
  if(length(unique(currentSet$ZIPCODESTUDENTSADDRESSZIPCODE))>1) students$ZipChange[i] = T
  if(length(unique(currentSet$DISTRICTCODEOFRESIDENCE))>1)       students$DORChange[i] = T
  if(any(unlist(students[i,c("StreetChange", "MuniChange", "ZipChange", "DORChange")]))){
    currentSet = currentSet[!duplicated(currentSet$Address),]
    studentIssues = rbind.data.frame(studentIssues, currentSet, stringsAsFactors = F)
    studentIssues = rbind.data.frame(studentIssues, emptyRow, stringsAsFactors = F)
  }
}

students$AnyChange = apply(students[,c("StreetChange", "MuniChange", "ZipChange")], MARGIN = 1, FUN = any)

write.csv(x = students[students$AnyChange,1:3], file = "StudentsWithAddressChanges.csv")

studentIssues = studentIssues[,c("version", "STUDENTIDSCHOOLDISTRICTSTUDENTID","FIRSTNAMESHORTSTUDENTSFIRSTNAME","LASTNAMESHORTSTUDENTSLASTNAME","Address")]
write.csv(x = studentIssues, file = "AllAddressChanges.csv", row.names = F)


