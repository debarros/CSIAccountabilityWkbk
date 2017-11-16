# DemographicChanges.R

files = list.files("J:/2016-2017/Level 0/student lite (demographics)", full.names = T)  
demog = vector(mode = "list", length = length(files))
nchar(files[1])


for(i in 1:length(files)){
  demog[[i]] = read.csv(file = files[i], header = F, stringsAsFactors = F)
  names(demog)[i] = substr(files[i],73,86)
}


demog.combo = rbindlist(l = demog, idcol = "version")
demog.combo = as.data.frame(demog.combo)
demog.combo$Address = paste0(demog.combo$V29, ", ",demog.combo$V31, ", ",demog.combo$V33)
students = data.frame(StudentID = unique(demog.combo$V4))
students$LastName = demog.combo$V5[match(students$StudentID, demog.combo$V4)]
students$FirstName = demog.combo$V6[match(students$StudentID, demog.combo$V4)]
students$StreetChange = F #V29
students$MuniChange = F #V31
students$ZipChange = F #V33
studentIssues = demog.combo[0,]
emptyRow = demog.combo[1,]
for(i in 1:ncol(emptyRow)){
  emptyRow[1,i] = ""
}
for(i in 1:nrow(students)){
  currentSet = demog.combo[demog.combo$V4 == students$StudentID[i],]
  if(length(unique(currentSet$V29))>1) students$StreetChange[i] = T
  if(length(unique(currentSet$V31))>1) students$MuniChange[i] = T
  if(length(unique(currentSet$V33))>1) students$ZipChange[i] = T
  if(any(unlist(students[i,c("StreetChange", "MuniChange", "ZipChange")]))){
    currentSet = currentSet[!duplicated(currentSet$Address),]
    studentIssues = rbind.data.frame(studentIssues, currentSet, stringsAsFactors = F)
    studentIssues = rbind.data.frame(studentIssues, emptyRow, stringsAsFactors = F)
  }
}

students$AnyChange = apply(students[,c("StreetChange", "MuniChange", "ZipChange")], MARGIN = 1, FUN = any)

write.csv(x = students[students$AnyChange,1:3], file = "StudentsWithAddressChanges.csv")

studentIssues = studentIssues[,c("version", "V5","V6","Address")]
write.csv(x = studentIssues, file = "AllAddressChanges.csv", row.names = F)


