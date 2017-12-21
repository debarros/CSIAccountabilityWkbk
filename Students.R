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

CohortIssues = UnEnrolledInCohort(Workbook, 6)
if(nrow(CohortIssues) > 0){
  print("There are cohort issues to resolve.  Check the file.")
  write.csv(CohortIssues, file = "CohortIssues.csv")
}


##################################
# Identify Off-Track Students ####
##################################

offtrack = Workbook.InCohort[Workbook.InCohort$`Grade.(leave.blank.if.no.longer.enrolled)` %in% c(9:12) & toupper(Workbook.InCohort$`Still.Enrolled?`) == "YES",]
offtrack = offtrack[,c("Local.ID.(optional)", "Last.Name", "First.Name", "Cohort.Year.(year.1st.entered.9th)", "Grade.(leave.blank.if.no.longer.enrolled)")]
offtrack$off = (offtrack$`Cohort.Year.(year.1st.entered.9th)` + offtrack$`Grade.(leave.blank.if.no.longer.enrolled)`) != schoolYear() + 9
write.csv(offtrack[offtrack$off,], file = "offtrack.csv")


