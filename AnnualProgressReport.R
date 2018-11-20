# AnnualProgressReport.R

#---------------------------#
#### Load and clean data ####
#---------------------------#

lastYear = schoolYear() - 1
lastDay = schoolYear("end", Sys.Date() - 365)

# Select the most recent student lite export from the relevant school year
studLite = read.csv(file.choose(), header = F, stringsAsFactors = F)
colnames(studLite) = dBtools::GetNiceColumnNames("STUDENT LITE", templates)

# Select the most recent school enrollment export from the relevant school year
enrollment = read.csv(file.choose(), header = F, stringsAsFactors = F)
colnames(enrollment) = dBtools::GetNiceColumnNames("SCHOOL ENTRY EXIT", templates)


# Convert dates to date format
enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE[enrollment$SCHOOLEXITDATE == ""] = NA 
enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE = as.Date(enrollment$SCHOOLEXITDATE)
enrollment$SCHOOLENTRYDATEENROLLMENTENTRYDATE = as.Date(enrollment$SCHOOLENTRYDATE)

# Set missing exit dates to the end of the year
enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE[is.na(enrollment$SCHOOLEXITDATE)] = lastDay

#-----------------------------------------------------------#
#### 1) School enrollment by Grade Level and School Year ####
#-----------------------------------------------------------#

# look for students who exited during graduation or later (ie stayed to the end of the year)
EOYstudents = enrollment[enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE > lastDay - 15,] 

# This produces the number of students in each grade at the end of the year
summary(as.factor(EOYstudents$ENROLLMENTGRADELEVELGRADELEVEL))
print(paste0("total = ", nrow(EOYstudents)))

#----------------------------------------#
# 2) Accountability cohort membership ####
#----------------------------------------#

# Who was in the cohort on BEDS Day?
BEDSday = BedsDate(lastYear)
studLite$cohort = Workbook$`Cohort.Year.(year.1st.entered.9th)`[match(studLite$STUDENTIDSCHOOLDISTRICTSTUDENTID, Workbook$`Local.ID.(optional)`)]
studLite$EnrolledOnBedsDay = F

# Check to see if there is at least 1 enrollment for the student that encompasses BEDS day
for(i in 1:nrow(enrollment)){
  if(enrollment$SCHOOLENTRYDATEENROLLMENTENTRYDATE[i] <= BEDSday & enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE[i] >= BEDSday){
    studLite$EnrolledOnBedsDay[studLite$STUDENTIDSCHOOLDISTRICTSTUDENTID == enrollment$STUDENTIDSCHOOLDISTRICTSTUDENTID[i]] = T
  }
}
table(studLite$cohort, studLite$EnrolledOnBedsDay)[,2]

# Of those, Who was left at the end of the year?
studLite$FinishedYear = studLite$STUDENTIDSCHOOLDISTRICTSTUDENTID %in% EOYstudents$STUDENTIDSCHOOLDISTRICTSTUDENTID
summary(as.factor(studLite$cohort[studLite$FinishedYear & studLite$EnrolledOnBedsDay]))

# Ergo, how many left during the year?  Just subtract


#--------------------------------------#
#### 3) Total cohort for graduation ####
#--------------------------------------#

# look at the Summary By Grad Cohort tab
# Use column N to get the number in the 4-year grad cohort
# Use column O to get the number in the 5-year grad cohort

# The following line gives the number of students in each cohort who were enrolled at the end of the year
table(studLite$cohort, studLite$FinishedYear)

# For additional students still in each cohort, just subtract


#---------------------------#
#### 4) Promotion Policy ####
#---------------------------#

# get this from the principal


#---------------------------------------------------------------------#
#### 5) Percent of students earning the required number of credits ####
#---------------------------------------------------------------------#

# This should be the percentage of students in the 1st year cohort who earned at least 5 credits 
# and in the second year cohort who earned at least 10 credits
# Credits need to be updated in the acct wkbk after summer school
RegentsEndDate = as.Date("2018-08-17")
Year1 = Workbook[Workbook$`Cohort.Year.(year.1st.entered.9th)` == 2017,]
Year1 = Year1[!(VbetterComp(Year1$Date.left.GTH < RegentsEndDate,TRUE)),] # remove students who left before August regents exams
nrow(Year1)
sum(Year1$Total.Credits.Earned >= 5)
mean(Year1$Total.Credits.Earned >= 5)

Year2 = Workbook[Workbook$`Cohort.Year.(year.1st.entered.9th)` == 2016,]
Year2 = Year2[!(VbetterComp(Year2$Date.left.GTH < RegentsEndDate,TRUE)),]
nrow(Year2)
sum(Year2$Total.Credits.Earned >= 10)
mean(Year2$Total.Credits.Earned >= 10)


#-----------------------------------------------------#
#### 6) Cohort 4-year grad rate (including August) ####
#-----------------------------------------------------#

# check the Summary by Grad Cohort tab


#-----------------------------------------------------#
#### 7) Cohort 5-year grad rate (including August) ####
#-----------------------------------------------------#

# check the Summary by Grad Cohort tab


#---------------#
#### 8) PSAT ####
#---------------#

# This section may no longer be needed.

PSATs = read.xlsx(xlsxFile = CollegeBoardLocation, sheet = "PSAT")
PSATs.relevant = PSATs[PSATs$Grade == 10 & PSATs$Year == lastYear,]

# No. of students in 10th grade during the PSAT
PSATdate = as.Date("2017-10-11") # search online to get this date
sum(enrollment$SCHOOLENTRYDATEENROLLMENTENTRYDATE <= PSATdate & enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE >= PSATdate & enrollment$ENROLLMENTGRADELEVELGRADELEVEL == 10)

#How many took it?
nrow(PSATs.relevant)
mean(as.numeric(PSATs.relevant$Read))
mean(as.numeric(PSATs.relevant$Math))


#--------------#
#### 9) SAT ####
#--------------#

# This section may no longer be needed.

SATs = read.xlsx(xlsxFile = "J:/SAT's/college_board_data.xlsx", sheet = "SAT")
# Because our students take the SAT either at the end of their junior year or the beginning of their senior year, 
# use the set of students who (1) were seniors during the relevant school year, 
# (2) who had enrolled before the deadline (in May) to take the June SAT during their junior year, 
# and (3) remained enrolled until after the first SAT administration in the Fall of their senior year (October).

SpringSATdeadline = as.Date("2016-05-25")
FallSATdate = as.Date("2016-10-01")

SATs.best = data.frame(StudentID = unique(SATs$ID))
SATs.best$reading = NA_integer_
SATs.best$math = NA_integer_
for(i in 1:nrow(SATs.best)){
  SATs.best$reading[i] = betterMax(SATs$Reading[SATs$ID == SATs.best$StudentID[i]])
  SATs.best$math[i] = betterMax(SATs$Math[SATs$ID == SATs.best$StudentID[i]])
}
studentset = data.frame(StudentID = studLite$STUDENTIDSCHOOLDISTRICTSTUDENTID[studLite$CURRENTGRADELEVELGRADELEVEL == 12])
studentset$exitdate = Workbook$Date.left.GTH[match(studentset$StudentID, Workbook$`Local.ID.(optional)`)]
studentset$entrydate = Workbook$Date.First.Enrolled.at.GTH[match(studentset$StudentID, Workbook$`Local.ID.(optional)`)]
studentset = studentset[!(VbetterComp(studentset$exitdate < FallSATdate, TRUE)),] #eliminate the students who left before the fall SAT date
studentset = studentset[!(VbetterComp(studentset$entrydate > SpringSATdeadline, TRUE)),] #eliminate the students who entered after the spring SAT registration deadline
SATs.best = SATs.best[SATs.best$StudentID %in% studentset$StudentID,]
#number of 12th graders who fit the criteria
nrow(studentset)
#number of students tested
nrow(SATs.best)

mean(SATs.best$reading)
mean(SATs.best$math)
