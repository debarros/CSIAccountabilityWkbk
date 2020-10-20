#Students.R

# This is for looking at problems with the cohorts, such as students missing transfer info
# For more information, see the file in the online operations manual at 
# Instructions > Data Reporting > CSI > Accountability Workbook > Identifying Graduation Cohort Issues

# Note: First run MainScript.R


#------------------------------------------#
#### Reconcile Workbook and PowerSchool ####
#------------------------------------------#

# This part finds discrepancies between PowerSchool and the Accountability Workbook in terms of which students are current
# Note that it does not handle the Active but Not Enrolled situation well just yet

psStudents = powerschoolraw                                                                     # Make a copy of the original data
psStudents$DOR = DORs$District.Name[match(psStudents$DistrictOfResidence, DORs$District.ID)]    # Get the DOR name
psStudents.high = psStudents[psStudents$grade_level >= 9,]
inconsistencies = FindInconsistentActiveStatus(psStudentsRaw = psStudents.high, Workbook = Workbook) # Identify inconsistencies
if(is.data.frame(inconsistencies)){                                                             # If there are any,
  write.csv(inconsistencies, paste0(OutFolder,"active and inactive inconsistencies.csv"))       # Output the results
  print("There were inconsistencies between the workbook and PowerSchool.  Check the file.")    # Notify the user
} else {
  print("There are no inconsistencies between the workbook and PowerSchool.  Yay!")
}


#------------------------------#
#### Identify Cohort Issues ####
#------------------------------#

CohortIssues = UnEnrolledInCohort(Workbook, 6)
CohortIssues$PS.comment = psStudents$ExitComment[match(CohortIssues$`Local.ID.(optional)`, psStudents$student_number)]
CohortIssues$DOR = psStudents$DOR[match(CohortIssues$`Local.ID.(optional)`, psStudents$student_number)]
colnames(CohortIssues) = c("Student ID", "Last", "First", "Cohort", "Date left", "Discharge Reason", "PS Comment", "DOR")
if(nrow(CohortIssues) > 0){
  print("There are cohort issues to resolve.  Check the file.")
  write.csv(CohortIssues, paste0(OutFolder,"Missing Transfer Info.csv"))
} else {
  print("There are no inactive students in any graduation cohort.  Yay!")
}


#-----------------------------------#
#### Identify Off-Track Students ####
#-----------------------------------#

Workbook.InCohort = Workbook[toupper(Workbook$`Included.in.Graduation.Cohort?`) == "YES",]          # limit to those in the grad cohort
offtrack = Workbook.InCohort[Workbook.InCohort$`Grade.(leave.blank.if.no.longer.enrolled)` %in% c(9:12) & toupper(Workbook.InCohort$`Still.Enrolled?`) == "YES",]
if(nrow(offtrack) > 0){
  offtrack = offtrack[,c("Local.ID.(optional)", "Last.Name", "First.Name", "Cohort.Year.(year.1st.entered.9th)", "Grade.(leave.blank.if.no.longer.enrolled)")]
  offtrack$off = (offtrack$`Cohort.Year.(year.1st.entered.9th)` + offtrack$`Grade.(leave.blank.if.no.longer.enrolled)`) != schoolYear() + 9
  offtrack = offtrack[offtrack$off,]
  offtrack$offBy = schoolYear() + 9 - (offtrack$`Cohort.Year.(year.1st.entered.9th)` + offtrack$`Grade.(leave.blank.if.no.longer.enrolled)`)
  offtrack = offtrack[offtrack$offBy > 0,]
  offtrack$off = NULL
  colnames(offtrack) = c("Student ID", "Last", "First", "Cohort", "Grade", "Off By")
  write.csv(offtrack, paste0(OutFolder,"offtrack.csv"))
  print("There are offtrack students.  Check the file.")
} else {
  print("There are no students off track to graduate on time.  Yay!")
}


#-------------------------------------------------------#
#### Identify Seniors not in the Current Grad Cohort ####
#-------------------------------------------------------#

Seniors = Workbook.InCohort[VbetterComp(Workbook.InCohort$`Grade.(leave.blank.if.no.longer.enrolled)`, 12) & toupper(Workbook.InCohort$`Still.Enrolled?`) == "YES",]
Seniors.other = Seniors[Seniors$`Cohort.Year.(year.1st.entered.9th)` != schoolYear() - 3,]
if(nrow(Seniors.other) > 0 ){
  Seniors.other = Seniors.other[,c("Local.ID.(optional)", "Last.Name", "First.Name", "Cohort.Year.(year.1st.entered.9th)")]
  colnames(Seniors.other) = c("Student ID", "Last", "First", "Cohort")
  write.csv(Seniors.other, paste0(OutFolder, "seniors not in the cohort.csv"))
  print("There are seniors who are not in the current graduating cohort.  Check the file.")
} else {
  print("All seniors are in the appropriate cohort.")
}



#------------------------------------------------------------#
#### Create a status table of students in the Grad Cohort ####
#------------------------------------------------------------#

gradCohort = Workbook.InCohort[Workbook.InCohort$`Cohort.Year.(year.1st.entered.9th)` == schoolYear() - 3,]
gradCohort$Status = ""
gradCohort$Status[!(VbetterComp(gradCohort$`Grade.(leave.blank.if.no.longer.enrolled)`, 12))]   = "Off Track"
gradCohort$Status[toupper(gradCohort$`Still.Enrolled?`) == "NO"]   = "Not Enrolled"
gradCohort$Status[VbetterComp(toupper(gradCohort$Discharge.Reason), "GRADUATED")]   = "Graduated"
gradCohort2 = gradCohort[,c("Local.ID.(optional)", "Last.Name", "First.Name", "Status")]
colnames(gradCohort2) = c("Student ID", "Last", "First", "Status")
gradCohort2 = gradCohort2[order(gradCohort2$Last, gradCohort2$First),]
write.csv(gradCohort2, paste0(OutFolder,"graduating cohort.csv"))


#-------------------------------------------#
#### Create a summary of the Grad Cohort ####
#-------------------------------------------#

GCSum = nrow(gradCohort)
GCSum = c(GCSum, sum(gradCohort$Status == "Not Enrolled"))
GCSum = c(GCSum, sum(gradCohort$Status == "Off Track"))
GCSum = c(GCSum, sum(gradCohort$Status == "Graduated"))
GCSum = c(GCSum, (GCSum[1] - GCSum[2] - GCSum[3]) / GCSum[1])
GCSum[1:4] = round(GCSum[1:4],0)
GCSum[5] = round(100 * GCSum[5], 2)
GCSum.words = paste0(GCSum, c(" students in the cohort", " are missing", " are off track", " already graduated", "% is the best possible grad rate"))
GCSum.words = c(paste0(schoolYear() - 3, " Cohort Summary"), GCSum.words)
write.csv(GCSum.words, paste0(OutFolder, "cohort summary.csv"))


