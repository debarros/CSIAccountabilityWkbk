#L2cohorts.R

#Compare export from the Total Cohort Detail from the SIRS 201 report top acct wkbk



#This file uses the "Excel 2007 format" option in L2.  
#The extra headers were deleted, and the exports combined into 1 file.
#A column was added for Cohort
L2 = read.xlsx(xlsxFile = "Total Cohort - Detail (1).xlsx", sheet = 1)

#limit to students in the grad cohort
wkbkInCohort = Workbook[Workbook$Included.in.Graduation.Cohort. %in% c("Yes","yes"),]


#add the 0 padded ID field
wkbkInCohort$CharID = as.character(wkbkInCohort$Local.ID..optional.)
for(i in 1:nrow(wkbkInCohort)){
  len = nchar(wkbkInCohort$CharID[i])
  if(len<9){
    wkbkInCohort$CharID[i] = paste0(paste0(rep("0",times = 9-len), collapse = ""),wkbkInCohort$CharID[i])
  }
}



#find students in L2 who are not in the wkbk at all
extraStudents = setdiff(L2$Student.ID, wkbkInCohort$CharID)
L2[L2$Student.ID %in% extraStudents,]
L2reduced = L2[!(L2$Student.ID %in% extraStudents),]

#find students who are in the wrong cohort
misplaced = as.data.frame(matrix(data = NA, nrow = 0, ncol = 4), stringsAsFactors = F)
colnames(misplaced) = c("LocalId","Name","L2Cohort","WkbkCohort")

for(i in 1:nrow(L2reduced)){
  LocalId = L2reduced$Student.ID[i]
  Name = L2reduced$Student[i]
  L2Cohort = L2reduced$Cohort[i]
  WkbkCohort = wkbkInCohort$Cohort.Year..year.1st.entered.9th.[match(x = LocalId, table = wkbkInCohort$CharID)]
  NewRow = data.frame(LocalId, Name, L2Cohort, WkbkCohort)
  if(L2Cohort != WkbkCohort){
    misplaced = rbind.data.frame(misplaced, NewRow)
  }
}

View(misplaced)


#find students who are not showing up in L2
wkbkInCurCohort = wkbkInCohort[wkbkInCohort$Cohort.Year..year.1st.entered.9th. > 2010,]
missingStudents = setdiff(wkbkInCurCohort$CharID, L2$Student.ID)
missingStudents = wkbkInCurCohort[wkbkInCurCohort$CharID %in% missingStudents,]
View(missingStudents[,2:4])

L2[grep(pattern = "Shed",x = L2$Student, ignore.case = T),]
