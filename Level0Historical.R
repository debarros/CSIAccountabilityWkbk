#Level0Historical.R

#---------------------------#
#### Fix historical data ####
#---------------------------#

#Subset to the relevant students - entered last year, but with a different cohort year
cohort = "2015"
idStart = paste0(substr(cohort,3,4), 1+as.integer(substr(cohort,3,4)))

StudentsOfInterest = Workbook[substr(as.character(Workbook$`Local.ID.(optional)`),1,4) == idStart & Workbook$`Cohort.Year.(year.1st.entered.9th)` != as.integer(cohort),]

View(StudentsOfInterest[,c(2:4,13)])

#Go through each student.  Look him up in L0H.  
#Check his data of entry into grade 9 against the first record actually showing an enrollment in 9th grade.
#Check the assessment records against the regents database.