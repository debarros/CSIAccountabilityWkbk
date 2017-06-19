#Students.R

#First run MainScript.R

#########################################
# Reconcile Workbook and PowerSchool ####
#########################################

#This script finds discrepancies between PowerSchool and the Accountability Workbook in terms of which students are current

#Export a ton of info from the Students table in PS
# psStudentsRaw = read.csv("PowerSchoolStudents.csv")
psStudentsRaw = powerschool

#Get a list of the students in PS 
InPS = psStudentsRaw$student_number

#Get a list of the students active in PS 
ActiveInPS = psStudentsRaw$student_number[psStudentsRaw$Enroll_Status == 0]

#Get a list of the students from the workbook 
InWorkbook = Workbook$`Local.ID.(optional)`

#Get a list of the students from the workbook who are active 
ActiveInWorkbook = Workbook$`Local.ID.(optional)`[(Workbook$`Still.Enrolled?` == "yes") ]

#Get a list of the students who are in both the Workbook and PS
inBoth = intersect(InWorkbook, InPS)

#Get a list of the students from the workbook who are inactive 
InactiveInWorkbook = setdiff(InWorkbook, ActiveInWorkbook)

#Get a list of the students who are enrolled in PS, appear in the wkbk, but are not enrolled in the wkbk
ActiveInPS.InactiveInWkbk = intersect(ActiveInPS, InactiveInWorkbook) 
#should be 0

#Get a list of students who are enrolled in PS but nowhere in the wkbk
ActiveInPS.NotInWorkbook = setdiff(ActiveInPS, InWorkbook)
#should be 0

#Get a list of students who are active in the workbook, appear in PS, but are not enrolled in PS
ActiveInWorkbook.InactiveInPS = setdiff(intersect(ActiveInWorkbook,InPS), ActiveInPS)
#should be 0

#Get a list of students who are active in the workbook, but are not in PS at all
ActiveInWorkbook.NotInPS = setdiff(ActiveInWorkbook, InPS)
#should be 0

#Compile information into one readable table
#write code here

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
offtrack$off = (offtrack$`Cohort.Year.(year.1st.entered.9th)` + offtrack$`Grade.(leave.blank.if.no.longer.enrolled)`) != 2025 # adjust this number
write.csv(offtrack[offtrack$off,], file = "offtrack.csv")
