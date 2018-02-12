# L2cohorts.R

# Compare export from the Total Cohort Detail from the SIRS 201 report to the acct wkbk
# This file uses the "Excel 2007 format" option in L2.  
# The extra headers were deleted, and the exports combined into 1 file.
# This should be rewritten to use the "Excel 2007 Data" option


# Read in data and format it ####

L2 = read.xlsx(xlsxFile = "Total Cohort - Detail.xlsx", sheet = 1)
L2$Cohort = as.integer(format(as.Date(L2$Date.Entry.Gr9, origin = "1899-12-30"), "%Y"))

# limit workbook to students in the grad cohort
wkbkInCohort = Workbook[grepl("yes", Workbook$`Included.in.Graduation.Cohort?`, T),]

# Add the 0 padded ID field
wkbkInCohort$CharID = as.character(wkbkInCohort$`Local.ID.(optional)`)
for(i in 1:nrow(wkbkInCohort)){
  len = nchar(wkbkInCohort$CharID[i])
  if(len<9){
    wkbkInCohort$CharID[i] = paste0(paste0(rep("0",times = 9-len), collapse = ""),wkbkInCohort$CharID[i])
  }
}

# Get a table of DOR's by student
StudentDOR = powerschoolraw[,c("student_number","DistrictOfResidence")]
StudentDOR$DORname = DORs$District.Name[match(StudentDOR$DistrictOfResidence, DORs$District.ID)]


# Find students in L2 who are not in the wkbk at all ####

extraStudents = setdiff(L2$Student.ID, wkbkInCohort$CharID)
L2reduced = L2[!(L2$Student.ID %in% extraStudents),]
if(length(extraStudents) > 0){
  extraStudents = L2[L2$Student.ID %in% extraStudents,]
  View(extraStudents)
} else {
  print("No students are completely missing from the workbook")
}


# Find students who are in the wrong cohort ####

# Set up a data.frame to hold the misplaced students
misplaced = as.data.frame(matrix(data = NA, nrow = 0, ncol = 4), stringsAsFactors = F)
colnames(misplaced) = c("LocalId","Name","L2Cohort","WkbkCohort")

for(i in 1:nrow(L2reduced)){
  LocalId = L2reduced$Student.ID[i]
  Name = L2reduced$Student[i]
  L2Cohort = L2reduced$Cohort[i]
  WkbkCohort = wkbkInCohort$`Cohort.Year.(year.1st.entered.9th)`[match(x = LocalId, table = wkbkInCohort$CharID)]
  NewRow = data.frame(LocalId, Name, L2Cohort, WkbkCohort)
  if(L2Cohort != WkbkCohort){
    misplaced = rbind.data.frame(misplaced, NewRow)
  }
}

if(nrow(misplaced) > 0){
  View(misplaced)
} else {
  print("There are no students assigned to the wrong cohort")
}



# Find students who are not showing up in L2 ####
wkbkInCurCohort = wkbkInCohort[wkbkInCohort$`Cohort.Year.(year.1st.entered.9th)` > schoolYear() - 5,]
missingStudents = setdiff(wkbkInCurCohort$CharID, L2$Student.ID)
if(length(missingStudents) > 0){
  missingStudents = wkbkInCurCohort[wkbkInCurCohort$CharID %in% missingStudents,1:4]
  missingStudents$DOR = StudentDOR$DORname[match(missingStudents$`Local.ID.(optional)`, StudentDOR$student_number)]
  missingStudents = missingStudents[order(missingStudents$DOR, missingStudents$Last.Name, missingStudents$First.Name),]
  rownames(missingStudents) <- NULL
  View(missingStudents)
} else {
  print("There are no students missing from Level 2.")
}


write.csv(missingStudents, paste0(OutFolder, "MissingFromL2Cohort.csv"))

