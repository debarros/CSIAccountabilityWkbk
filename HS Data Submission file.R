# HS Data Submission file
# This script prepares the new version of the accountability workbook
# This is the form that needs to get submitted starting in September 2018

lastDay = schoolYear(x = "end", y = Sys.Date() - 365)
bedsDay = BedsDate(schoolYear() - 1)
termID = schoolYear(x = "termID", y = bedsDay)

# Read in the most up-to-date demographics export from PowerSchool from the relevant year
studentLite = read.csv(file.choose(), header = F)
colnames(studentLite) = GetNiceColumnNames(sheet = "STUDENT LITE", templates = templates)


# Read in the most up-to-date enrollment export from PowerSchool from the relevant year
EnrollExt = read.csv(file.choose(), header = F, stringsAsFactors = F)
colnames(EnrollExt) = GetNiceColumnNames(sheet = "SCHOOL ENTRY EXIT", templates = templates)
EnrollExt$SCHOOLENTRYDATEENROLLMENTENTRYDATE = as.Date(EnrollExt$SCHOOLENTRYDATEENROLLMENTENTRYDATE)
EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE[EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE == ""] = as.character(lastDay)
EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE = as.Date(EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE)
EnrollExt$CoversBedsDay = EnrollExt$SCHOOLENTRYDATEENROLLMENTENTRYDATE < bedsDay & EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE > bedsDay
bedsStudents = EnrollExt$STUDENTIDSCHOOLDISTRICTSTUDENTID[EnrollExt$CoversBedsDay]
bedsStudents = unique(bedsStudents)


# Read in the most up-to-date program services export from PowerSchool from the relevant year
progFact = read.csv(file.choose(), header = F, stringsAsFactors = F)
colnames(progFact) = GetNiceColumnNames(sheet = "PROGRAMS FACT", templates = templates)
dateVars = c("SCHOOLYEARDATE", "BEGINNINGDATEPROGRAMSERVICEENTRYDATE", "ENDINGDATEPROGRAMSERVICEENDDATE")
for(i in dateVars){ progFact[,i] = as.Date(progFact[,i]) }
progFact$ENDINGDATEPROGRAMSERVICEENDDATE[is.na(progFact$ENDINGDATEPROGRAMSERVICEENDDATE)] = as.Date(lastDay)


# Read in the crosswalk data (mapping old fields to new ones)
crosswalk = read.xlsx(
  xlsxFile = "\\\\stuthin2\\Data\\Accountability Spreadsheet\\guidance documents\\Old to new crosswalk.xlsx", 
  sheet = "Sheet2")

# Make a copy of the workbook.  This will become the output.
submittable = Workbook

# Fix cohort inclusion stuff (in case the workbook has already been updated with students who transferred out after EOY)
for(i in 1:nrow(submittable)){
  if(!is.na(submittable$Date.left.GTH[i])){                # If the student left
    if(submittable$Date.left.GTH[i] > lastDay){            # If they left after the last day of the school year
      if(submittable$Discharge.Reason[i] != "Graduated"){  # If they didn't graduate
        submittable$Discharge.Reason[i] = ""
        submittable$`Still.Enrolled?`[i] = "yes"
        gradlvl = studentLite$CURRENTGRADELEVEL[match(submittable$`Local.ID.(optional)`[i], studentLite$STUDENTIDSCHOOL)]
        if(length(gradlvl) != 1){
          stop(paste0(length(gradlvl), " grade levels for student ", submittable$`Local.ID.(optional)`[i]))
        }
        submittable$`Grade.(leave.blank.if.no.longer.enrolled)`[i] = gradlvl
        submittable$Date.left.GTH[i] = NA
        submittable$`Included.in.Accountability.Cohort?`[i] = "Yes"
        submittable$`If.No,.Why.Not?`[i] = NA
        submittable$`Included.in.Graduation.Cohort?`[i] = "Yes"
        submittable$`If.No,.Why.Not?2`[i] = NA
      } # /if they didn't graduate
    } # /if
  } # /if
} # /for


# The section below was written before new rules for including grade levels were released
# # Fix grade levels (in case workbook has already been updated for the new year)
# for(i in 1:nrow(submittable)){
#   curStuID = submittable$`Local.ID.(optional)`[i]
#   if(curStuID %in% studentLite$STUDENTIDSCHOOLDISTRICTSTUDENTID){                 # If student was at GTH during relevant school year
#     fixit = is.na(submittable$Date.left.GTH[i])                                   # Fix if no exit date
#     if(!fixit){                                                                   # If there is an exit date,
#       fixit = submittable$Date.left.GTH[i] > lastDay                              # Fix if student exited after EOY
#       if(submittable$Discharge.Reason[i] == "Graduated"){fixit = T}             # Fix if student graduated during the school year
#     } # /if
#     if(fixit){                                                                    # If it needs fixing, do it
#       gradlvl = studentLite$CURRENTGRADELEVEL[match(curStuID, studentLite$STUDENTIDSCHOOL)]
#       submittable$`Grade.(leave.blank.if.no.longer.enrolled)`[i] = gradlvl
#     } # /if
#   } # /if
# } # /for


# Fix grade levels
for(i in 1:nrow(submittable)){
  curStuID = submittable$`Local.ID.(optional)`[i]
  if(curStuID %in% studentLite$STUDENTIDSCHOOLDISTRICTSTUDENTID){                         # If student at GTH in relevant school year
    gradlvl = studentLite$CURRENTGRADELEVEL[match(curStuID, studentLite$STUDENTIDSCHOOL)] # grab the grade level
    submittable$`Grade.(leave.blank.if.no.longer.enrolled)`[i] = gradlvl                  # put it in the output object
  } # /if
} # /for


# Fix the column names using the crosswalk, then remove unnecessary columns and add blank ones
columnNames = colnames(submittable)
for(i in 1:length(columnNames)){
  if(columnNames[i] %in% crosswalk$Old.Format.as.read.by.R){
    x = crosswalk$September.2020.Format[VbetterComp(crosswalk$Old.Format.as.read.by.R, columnNames[i])]
    if(length(x) != 1) {
      print(i)
      print(x)
    }
    columnNames[i] = x
  }
}
colnames(submittable) = columnNames
submittable = submittable[,columnNames %in% crosswalk$September.2020.Format]
moreColumns = setdiff(crosswalk$September.2020.Format, columnNames)
submittable[,moreColumns] = NA



#--------------------------------------------------#
#### Load stuff into the columns in moreColumns ####
#--------------------------------------------------#

# Homeless - mark as No.  Then mark as Yes for any student who was homeless on BEDS day last year
# Use the date range of the 8262 program service code to determine whether the student was homeless on BEDS day

homelessness = progFact[progFact$PROGRAMSCODEPROGRAMSERVICECODE == 8262,]
homelessness = homelessness[homelessness$BEGINNINGDATEPROGRAMSERVICEENTRYDATE < bedsDay,]
homelessness = homelessness[homelessness$ENDINGDATEPROGRAMSERVICEENDDATE > bedsDay,]
homelessIDs = homelessness$STUDENTIDSCHOOLDISTRICTSTUDENTID
submittable$Homeless = "No"
submittable$Homeless[submittable$`Local ID` %in% homelessIDs] = "Yes"


# Number of Credits during the relevant year - use F2 to calculate

curYearF2 = F2[F2$TermID >= termID & F2$TermID < termID + 100,]
for(i in 1:nrow(submittable)){
  curStuID = submittable$`Local ID`[i]
  curStuCred = curYearF2[curYearF2$`[1]Student_Number` == curStuID,]
  if(nrow(curStuCred) == 0){
    submittable$`Number of Credits Earned during the School Year of Interest`[i] = 0
  } else {
    submittable$`Number of Credits Earned during the School Year of Interest`[i] = sum(curStuCred$EarnedCrHrs)
  }
}


# PSAT - Get best PSAT score for each student

# Make a copy of the PSAT object
PSATraw.xlsx <- PSAT.raw

# Ensure that the data is of a numeric type. Empty cells can cause them to be read as character vectors.
PSATraw.xlsx$Read  <- as.integer(PSATraw.xlsx$Read)
PSATraw.xlsx$Math  <- as.integer(PSATraw.xlsx$Math)
PSATraw.xlsx$Write <- as.integer(PSATraw.xlsx$Write)

# Add the subscores together to get the total score
PSATraw.xlsx$Score <- rowSums(PSATraw.xlsx[,c("Read","Math","Write")], na.rm = TRUE)

# Sort by ID, then Score (descending)
PSATsort <- PSATraw.xlsx[with(PSATraw.xlsx, order(ID, -Score)),]

# Add rank of score by student
PSATsort$Rank[1] <- 1
for (i in 2:length(PSATsort$ID)) {
  if (PSATsort$ID[i] == PSATsort$ID[i-1]) {
    PSATsort$Rank[i] <- PSATsort$Rank[i-1] + 1
  }
  else {
    PSATsort$Rank[i] <- 1
  }
}

# Remove rows that aren't the best
PSATsort <- PSATsort[PSATsort$Rank == 1,]

# Load scores into the output object
submittable$`PSAT Administration Year` = PSATsort$Year[match(submittable$`Local ID`, PSATsort$ID, nomatch = "")]
submittable$`PSAT Mathematics Score` = PSATsort$Math[match(submittable$`Local ID`, PSATsort$ID, nomatch = "")]
submittable$`PSAT Reading and Writing Score` = PSATsort$Read[match(submittable$`Local ID`, PSATsort$ID, nomatch = "")]




# SAT - get best SAT score for each student

# Make a copy of the data
SATraw.xlsx <- SAT.raw

# Ensure that the data is of a numeric type. Empty cells can cause them to be read as character vectors.
SATraw.xlsx$Reading  <- as.integer(SATraw.xlsx$Reading)
SATraw.xlsx$Math     <- as.integer(SATraw.xlsx$Math)
SATraw.xlsx$Writing  <- as.integer(SATraw.xlsx$Writing)

# Add the subscores together to get the total score
SATraw.xlsx$Score <- rowSums(SATraw.xlsx[,c("Reading","Math","Writing")], na.rm = TRUE)

# Sort by ID, then Score (descending)
SATsort <- SATraw.xlsx[with(SATraw.xlsx, order(ID, -Score)),]

# Add rank of score by student
SATsort$Rank[1] <- 1
for (i in 2:length(SATsort$ID)) {
  if (SATsort$ID[i] == SATsort$ID[i-1]) {
    SATsort$Rank[i] <- SATsort$Rank[i-1] + 1
  }
  else {
    SATsort$Rank[i] <- 1
  }
}

# Remove rows that aren't the best scores
SATsort <- SATsort[SATsort$Rank == 1,]

# Load scores into the output object
submittable$`SAT Administration Year` = SATsort$Year[match(submittable$`Local ID`, SATsort$ID, nomatch = "")]
submittable$`SAT Mathematics Score` = SATsort$Math[match(submittable$`Local ID`, SATsort$ID, nomatch = "")]
submittable$`SAT Reading and Writing Score` = SATsort$Reading[match(submittable$`Local ID`, SATsort$ID, nomatch = "")]



# CLEP - 
# NOTE - This section needs to be rewritten to allow for multiple CLEP scores to be entered for one student
CLEP = CLEP.raw                                 # Make a copy
CLEP = CLEP[order(CLEP$Score, decreasing = T),] # Put lowest scores at the bottom
CLEP = CLEP[!duplicated(CLEP$Student.ID),]      # If a student has multiple scores, keep the top one

submittable$`CLEP Exam 1` = ""
submittable$`CLEP Score 1` = ""

# Put clep scores in the output object
for(i in 1:nrow(CLEP)){
  submittable$`CLEP Exam 1`[submittable$`Local ID` == CLEP$Student.ID[i]] = CLEP$Exam[i]
  submittable$`CLEP Score 1`[submittable$`Local ID` == CLEP$Student.ID[i]] = CLEP$Score[i]
}


# AP - load all (best) AP scores

# Make a copy of the data
AP = AP.raw

# Sort by ID, then Exam, then Score (descending)
AP <- AP[with(AP, order(ID, Exam, -Score)),]

# If there are duplicate scores, remove the lower ones
AP$ID.Exam = paste0(AP$ID, ".", AP$Exam)
AP <- AP[!duplicated(AP$ID.Exam),]

# Add count of score by student
AP$Count = 1
for (i in 2:nrow(AP)) {
  if (AP$ID[i] == AP$ID[i - 1]) {
    AP$Count[i] <- AP$Count[i - 1] + 1
  }
  else {
    SATsort$Rank[i] <- 1
  }
}

# Determine the number of columns needed and add them to the output object
colsneeded = max(AP$Count)
for(i in 1:colsneeded){
  submittable[,paste0("AP Exam ", i)] = ""
  submittable[,paste0("AP Exam ", i, " Score")] = ""
}

# Load scores into the output object
for(i in 1:nrow(AP)){
  curStuID = AP$ID[i]
  curStuRow = which(submittable$`Local ID` == curStuID)
  curAPCol = AP$Count[i]
  submittable[curStuRow, paste0("AP Exam ", curAPCol)] = AP$Exam[i]
  submittable[curStuRow, paste0("AP Exam ", curAPCol, " Score")] = AP$Score[i]
}



# College Course - If a student has credit for a college course, enter Yes.  Otherwise, No.

submittable$`College Course` = "No"
Workbook$CollegeCourse = "No"
Workbook$CollegeCourse[!is.na(Workbook$`College.Course.Name.(must.have.passed)`)] = "Yes"
submittable$`College Course` = Workbook$CollegeCourse[match(submittable$`Local ID`, Workbook$`Local.ID.(optional)`)]


output = DFna.to.empty(submittable)
output = output[output$`Discharge Reason` != "never attended",] # remove students who never attended

# Output the submittable file
write.csv(x = output, file = paste0(OutFolder, "Green Tech High HS Data Submission.csv"), row.names = F)





#----------------------------------------------------------------#
#### Now, some more stuff to get the data for the APPR tables ####
#----------------------------------------------------------------#
output.gradcohort = output[output$`Included in Graduation Cohort` == "Yes",]


#---------------------------------------------------------------------#
#### APPR Table - School Enrollment by Grade Level and School Year ####
#---------------------------------------------------------------------#

# To get enrollment by grade level on BEDS Day, limit the file to those who were enrolled on BEDS day
output.BEDSDay = output[output$`Local ID` %in% bedsStudents,]
summary(as.factor(output.BEDSDay$`Grade in Current School Year `))


#-------------------------------------------------------------------#
#### APPR Table - Fourth-Year High School Accountability Cohorts ####
#-------------------------------------------------------------------#

# The next line shows the number of students who were enrolled on BEDS Day, broken down by whether they are now in the acct cohort
table(output.BEDSDay$`Cohort Year`, output.BEDSDay$`Included in Accountability Cohort`) 
table(output$`Cohort Year`, output$`Included in Accountability Cohort`) # This is the number in the acct cohort as of June 30th


#--------------------------------------------------------------------#
#### APPR Table - Fourth & Fifth Year Total Cohort for Graduation ####
#--------------------------------------------------------------------#

output.gradcohort$Graduated = output.gradcohort$`Discharge Reason` == "Graduated"
output.gradcohort$Missing = (!output.gradcohort$Graduated) & (output.gradcohort$`Still Enrolled as of August 2019?` == "no")
output.gradcohort$Status = "Enrolled"
output.gradcohort$Status[output.gradcohort$Missing] = "Missing"
output.gradcohort$Status[output.gradcohort$Graduated] = "Graduated"
table(output.gradcohort$`Cohort Year`, output.gradcohort$Status)
# Note that the table separate Graduated from Enrolled, but they should be combined for column (a) in the table.


#---------------------------------------------------------------------------------------#
#### APPR Table - Percent of Students in 1st & 2nd Year Cohort Earning Reqd Credits  ####
#---------------------------------------------------------------------------------------#
# Promotion
output.gradcohort$cred5 = output.gradcohort$`Number of Credits Earned as of August 2019` >= 5
output.gradcohort$cred10 = output.gradcohort$`Number of Credits Earned as of August 2019` >= 10
promotion = as.data.frame.matrix(table(output.gradcohort$`Cohort Year`, output.gradcohort$cred5))
promotion = cbind.data.frame(promotion, as.data.frame.matrix(table(output.gradcohort$`Cohort Year`, output.gradcohort$cred10)))
colnames(promotion) = c("Not5", "Earned5", "Not10", "Earned10")
promotion$Total = promotion$Not5 + promotion$Earned5
promotion$Rate5 = 100 * promotion$Earned5 / promotion$Total
promotion$Rate10 = 100 * promotion$Earned10 / promotion$Total
print(promotion)
# Note: use the Rate5 for the most recent cohort and Rate10 for the second most recent


#------------------------------------------------------------------------------------------#
#### APPR Table - Percent of Students in their Second Year Passing Three Regents Exams  ####
#------------------------------------------------------------------------------------------#
# Get the best scores for each type of exam
output.gradcohort$BestAlg1   = VbetterMax(as.numeric(output.gradcohort$`Regents Algebra I Common Core Scaled Score`), 
                                        as.numeric(output.gradcohort$`Regents Integrated Algebra Scaled Score`))
output.gradcohort$BestAlg2   = VbetterMax(as.numeric(output.gradcohort$`Regents Algebra II Common Core Scaled Score`), 
                                        as.numeric(output.gradcohort$`Regents Algebra 2/Trig Scaled Score`))
output.gradcohort$BestGeom   = VbetterMax(as.numeric(output.gradcohort$`Regents Geometry Common Core Scaled Score`), 
                                        as.numeric(output.gradcohort$`Regents Geometry Scaled Score`))
output.gradcohort$BestEarth  = as.numeric(output.gradcohort$`Regents Earth Science Scaled Score`)
output.gradcohort$BestChem   = as.numeric(output.gradcohort$`Regents Chemistry Scaled Score`)
output.gradcohort$BestBio    = as.numeric(output.gradcohort$`Regents Living Environment Scaled Score`)
output.gradcohort$BestPhys   = as.numeric(output.gradcohort$`Regents Physics Scaled Score`)
output.gradcohort$BestUS     = as.numeric(output.gradcohort$`Regents U.S. History Scaled Score`)
output.gradcohort$BestGlobal = VbetterMax(as.numeric(output.gradcohort$`Regents Global History Scaled Score`), 
                                          as.numeric(output.gradcohort$`Regents Transition Global History Scaled Score`))
output.gradcohort$BestELA    = VbetterMax(as.numeric(output.gradcohort$`Regents ELA Common Core Scaled Score`), 
                                       as.numeric(output.gradcohort$`Regents English Scaled Score`))

# Get the count of exams passed by subject area
output.gradcohort$MathExamsPassed = VbetterGreater(output.gradcohort$BestAlg1, 64) + 
  VbetterGreater(output.gradcohort$BestAlg2, 64) + 
  VbetterGreater(output.gradcohort$BestGeom, 64)
output.gradcohort$ELAExamsPassed = VbetterGreater(output.gradcohort$BestELA, 64) + 0
output.gradcohort$SStExamsPassed = VbetterGreater(output.gradcohort$BestUS, 64) + 
  VbetterGreater(output.gradcohort$BestGlobal, 64)
output.gradcohort$SciExamsPassed = VbetterGreater(output.gradcohort$BestEarth, 64) + 
  VbetterGreater(output.gradcohort$BestBio, 64) + 
  VbetterGreater(output.gradcohort$BestChem, 64) + 
  VbetterGreater(output.gradcohort$BestPhys, 64)

# Compile the total required exams passsed
output.gradcohort$BaseExamsPassed = VbetterGreater(output.gradcohort$MathExamsPassed, 0) + 
  VbetterGreater(output.gradcohort$ELAExamsPassed, 0) + 
  VbetterGreater(output.gradcohort$SStExamsPassed, 0) + 
  VbetterGreater(output.gradcohort$SciExamsPassed, 0)
output.gradcohort$ExtraExamsPassed = VbetterGreater(output.gradcohort$MathExamsPassed, 1) + 
  VbetterGreater(output.gradcohort$ELAExamsPassed, 1) + 
  VbetterGreater(output.gradcohort$SStExamsPassed, 1) + 
  VbetterGreater(output.gradcohort$SciExamsPassed, 1)
output.gradcohort$RequiredExamsPassed = output.gradcohort$BaseExamsPassed + (output.gradcohort$ExtraExamsPassed > 0)
output.gradcohort$RequiredExamsPassed.3 = output.gradcohort$RequiredExamsPassed > 2

# Compile the info
pass3exams = as.data.frame.matrix(table(output.gradcohort$`Cohort Year`, output.gradcohort$RequiredExamsPassed.3))
colnames(pass3exams) = c("Nope", "Yep")
pass3exams$Total = pass3exams$Nope + pass3exams$Yep
pass3exams$Rate = 100 * pass3exams$Yep / pass3exams$Total
# Note: only use the row that corresponds to the 2nd year cohort.  Other data is historical.



#----------------------------------------------------------------------------------------#
#### APPR Table - Percent of Students in Grad Cohort who Graduated After 4 & 5 Years  ####
#----------------------------------------------------------------------------------------#
# Grad Rates
gradRates = table(output.gradcohort$`Cohort Year`, output.gradcohort$`Discharge Reason` == "Graduated")
gradRates = as.data.frame.matrix(gradRates)
colnames(gradRates) = c("DidNot", "Did")
gradRates$Total = gradRates$DidNot + gradRates$Did
gradRates$Rate = 100 * gradRates$Did / gradRates$Total
print(gradRates)


#-----------------------------------------------------#
#### APPR Table - Pathway Students & Pathway Exams ####
#-----------------------------------------------------#

# This section does not make any sense.
studentLite$CAREERPATHCODE
summary(studentLite$CAREERPATHCODE)
studentLite[studentLite$CAREERPATHCODE == "CTE",]

#----------------------------------------------#
#### APPR Table - College Prep by Indicator ####
#----------------------------------------------#

# passed an AP

# Passed a CLEP

# Passed a college course

# Achieved college and career readiness

# Did at least 1 of them



#-----------------------------------------------------#
#### APPR Table - CCCRI Performance by Cohort Year ####
#-----------------------------------------------------#

#Don't know how to do this

#-------------------------------------------------------------------------------------------#
#### APPR Table - CCRI of Fourth-Year Total Cohort by Charter School and School District ####
#-------------------------------------------------------------------------------------------#

#Don't know how to do this

#-------------------------------------------------------------#
#### APPR Table - Matriculation Rates of Graduates by Year ####
#-------------------------------------------------------------#

# Matriculation rates
output.ontimegrads = output.gradcohort[output.gradcohort$Graduated,]
output.ontimegrads$deadline = as.Date(paste0(4 + as.numeric(output.ontimegrads$`Cohort Year`), "-08-31"))
output.ontimegrads = output.ontimegrads[output.ontimegrads$deadline > output.ontimegrads$`Date Left School (if No Longer Enrolled)`,]
matric = as.data.frame.matrix(table(output.ontimegrads$`Cohort Year`, output.ontimegrads$`College Matriculation` == "y"))
colnames(matric) = c("No", "Yes")
matric$Total = matric$No + matric$Yes
matric$Rate = 100 * matric$Yes / matric$Total
print(matric)


