#Students.R

# This is for looking at problems with the cohorts, such as students missing transfer info
# For more information, see the file in the online operations manual at 
# Instructions > Data Reporting > CSI > Accountability Workbook > Identifying Graduation Cohort Issues

# Note: First run MainScript.R


#########################################
# Reconcile Workbook and PowerSchool ####
#########################################

#This script finds discrepancies between PowerSchool and the Accountability Workbook in terms of which students are current
 
psStudents = powerschoolraw                                                                   # Make a copy of the original data
inconsistencies = FindInconsistentActiveStatus(psStudents, Workbook)                          # Identify inconsistencies
if(is.data.frame(inconsistencies)){                                                           # If there are any,
  write.csv(inconsistencies, "active and inactive inconsistencies.csv")                       # Output the results
  print("There were inconsistencies between the workbook and PowerSchool.  Check the file.")  # Notify the user
}


#############################
# Identify Cohort Issues ####
#############################

Workbook.InCohort = Workbook[toupper(Workbook$`Included.in.Graduation.Cohort?`) == "YES",]
unique(Workbook.InCohort$`Cohort.Year.(year.1st.entered.9th)`)
Workbook.InCohort.Unenrolled = Workbook.InCohort[toupper(Workbook.InCohort$`Still.Enrolled?`) == "NO",]
unique(Workbook.InCohort.Unenrolled$`Cohort.Year.(year.1st.entered.9th)`)
Workbook.Problems = Workbook.InCohort.Unenrolled[!VbetterComp(toupper(Workbook.InCohort.Unenrolled$Discharge.Reason), "GRADUATED"),]
unique(Workbook.Problems$`Cohort.Year.(year.1st.entered.9th)`)
Workbook.Problems = Workbook.Problems[Workbook.Problems$`Cohort.Year.(year.1st.entered.9th)` >= 2012,]
Workbook.Problems = Workbook.Problems[,c("Local.ID.(optional)","Last.Name","First.Name","Cohort.Year.(year.1st.entered.9th)","Date.left.GTH","Discharge.Reason")]
# Note that Workbook.Problems doesn't mean that there is a problem with the workbook
# This just refers to the students who are in the cohort, but not enrolled
write.csv(Workbook.Problems, file = "workbookproblems.csv")


##################################
# Identify Off-Track Students ####
##################################

offtrack = Workbook.InCohort[Workbook.InCohort$`Grade.(leave.blank.if.no.longer.enrolled)` %in% c(9:12) & toupper(Workbook.InCohort$`Still.Enrolled?`) == "YES",]
offtrack = offtrack[,c("Local.ID.(optional)", "Last.Name", "First.Name", "Cohort.Year.(year.1st.entered.9th)", "Grade.(leave.blank.if.no.longer.enrolled)")]
offtrack$off = (offtrack$`Cohort.Year.(year.1st.entered.9th)` + offtrack$`Grade.(leave.blank.if.no.longer.enrolled)`) != schoolYear() + 9
write.csv(offtrack[offtrack$off,], file = "offtrack.csv")


