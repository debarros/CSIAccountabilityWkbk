# Regents.R
# This is for reconciling regents scores in the regents database, powerschool, and the accountability workbook
# Note: Run the MainScript first.

# Save the raw versions in case the raw data is needed later (e.g., restarting this script from the top without rerunning MainScript)
RegentsDB = RegentsDBraw
Workbook.sub = Workbook
PowerSchool = powerschoolraw


#------------------------------------#
#### Set the exams and categories ####
#------------------------------------#


# if new exams have been introduced, you'll need to add them here
# Even better, this should be updated in the test name tab of PowerSchoolAll.xlsx
Exams = c("ELAOld","AlgOld","GeomOld","Trig","Bio","Earth","Chem","Phys","US","Global","ELACC","AlgCC","GeomCC", "Alg2CC", "GlobTrans")
nExam = length(Exams)
Categories = c("Alg","Geom","Trig","Bio","Earth","Chem","Phys","US","Global","ELA")
nCat = length(Categories)



#---------------------------------------------------------#
#### Make Workbook.sub score matrix and session matrix ####
#---------------------------------------------------------#

# Remove blank rows and unnecessary columns
# this line should keep the student ID and all of the regents scaled scores, performance levels, and exam year/terms
column_names = expand.grid(c("Scaled.Score", "Performance.Level", "Exam.Year.and.Term"),
                           c("English", "Algebra", "Geometry", "Trig", "Bio", 
                             "Earth.Science", "Chemistry", "Physics", "US.History", 
                             "Global", "Common.Core.ELA", "Common.Core.Algebra", 
                             "Common.Core.Geometry", "Common.Core.Algebra.2", "Global.Transition"))
column_names = c("Local.ID.(optional)", apply(column_names[,c(2,1)], 1, paste0, collapse = ""))

Workbook.sub = Workbook.sub[,column_names]  # subset the workbook to the relevant columns
names(Workbook.sub)[1] = "ID"               # change the first column to be named ID
studentlist = as.double(Workbook.sub[,1])   # this needs to be a double precision because some ID's are very long
n = length(studentlist)                     # this is the number of students

# Set up matrices to hold scores and sessions from the workbook, using student ID's as row names and exam names as column names
wkbkMatrix = matrix(integer(0), n, nExam, F, list(studentlist, Exams))
wkbkSessions = matrix(character(0), n, nExam, F, list(studentlist, Exams))

# Load in the actual scores and sessions
cols = 3*(1:nExam)-1  #the scores are in columns 2, 5, 8, etc.
for (i in studentlist){
  wkbkMatrix[which(rownames(wkbkMatrix) == i),] = as.integer(Workbook.sub[which(Workbook.sub$ID == i),cols])
  wkbkSessions[which(rownames(wkbkSessions) == i),] = as.character(Workbook.sub[which(Workbook.sub$ID == i),cols+2])
} # /for


#-------------------------------------------------------------#
#### Make Regents Database score matrix and session matrix ####
#-------------------------------------------------------------#

names(RegentsDB)[1] = "ID"  # change the first column to be named ID
# Set up the matrices to hold scores and sessions from the database with student IDs and rownames and exam names as column names
dbMatrix = matrix(integer(0), n, nExam, F, list(studentlist, Exams))
dbSessions = matrix(character(0), n, nExam, F, list(studentlist, Exams))

# load in the actual scores and sessions
for (i in studentlist){
  dbMatrix[which(rownames(dbMatrix) == i),] = as.integer(RegentsDB[which(RegentsDB$ID == i),2*(1:nExam)])
  dbSessions[which(rownames(dbSessions) == i),] = as.character(RegentsDB[which(RegentsDB$ID == i),(2*(1:nExam)+1)])
} # /for
# Note: if you get warnings here, there is likely a problem with the order of the columns in the database output

# Are there other problems?  Check it out:
#  studentlist[!(studentlist %in% RegentsDB$ID)]
#  RegentsDB$ID[!(RegentsDB$ID %in% studentlist)]
#  View(Workbookraw.xlsx[Workbookraw.xlsx$`Local.ID.(optional)` %in% RegentsDB$ID[!(RegentsDB$ID %in% studentlist)],])


#-------------------------------------------------#
#### Compare Regents Database and Workbook     ####
#-------------------------------------------------#

# Create a matrix with  the best scores
CompareMatrix = CreateCompareMatrix(studentlist, Exams, dbMatrix, wkbkMatrix)

# Create a list of things that need to be updated in the database
if(sum(!MbetterComp(CompareMatrix, dbMatrix) & !is.na(CompareMatrix))){                    # if any best scores not in the database
  temp = which(!MbetterComp(CompareMatrix, dbMatrix) & !is.na(CompareMatrix), arr.ind = T) # create matrix of locations (row and column)
  badDB = data.frame(ID = rownames(temp), Exam = Exams[temp[,2]])                          # create a list of the... something?
  write.csv(badDB,file = paste0(OutFolder,"ScoresMissingFromDataBase.csv"))                # export the file
  print("Resolve the scores missing from the Database")
} else {
  print("No scores missing from the Database")
}
# Note:
#     Take a look at the ScoresMissingFromDatabase.csv file
#     Those are instances in which a student has a score in the workbook but not in the database

# if there are any best scores not in the workbook, create output to be pasted into the Workbook
if(sum(!MbetterComp(CompareMatrix, wkbkMatrix) & !is.na(CompareMatrix)) > 0){ 
  wkbkOutput = data.frame(ID = rownames(CompareMatrix)) # set up the output object
  temp = CompareMatrix                                  # create a temporary version of the best scores matrix
  rownames(temp) = NULL                                 # get rid of the rownames in temp so it can be merged with the output object
  wkbkOutput = cbind(wkbkOutput, temp)                  # merge the best scores with the output object, so that it has student ID's
  rownames(wkbkMatrix) = NULL                           # get rid of row names from everything
  rownames(dbMatrix) = NULL
  rownames(wkbkSessions) = NULL
  rownames(dbMatrix) = NULL
  
  for (i in 1:nExam){                                                        # for each exam,
    wkbkOutput[,nExam+1+i] = NA                                              # add a column for it in the output object
    grabRows = which(wkbkOutput[,1+i] == wkbkMatrix[,i])                     # find which scores are consistent with the Workbook.sub
    wkbkOutput[grabRows,nExam+1+i] = wkbkSessions[grabRows,i]                # get sessions the Workbook and insert in the output object
    grabRows = which(wkbkOutput[,1+i] == dbMatrix[,i])                       # find which scores are consistent with the database
    wkbkOutput[grabRows,nExam+1+i] = dbSessions[grabRows,i]                  # get sessions from database and insert in output object
    names(wkbkOutput)[nExam+1+i] = paste0(names(wkbkOutput)[1+i]," Session") # name new column by test name plus the word session
  } #end of for loop
  
  Perf = matrix(NA, n, nExam)                                        # create empty variables to serve as the performance index columns
  Perf = data.frame(Perf)                                            # convert it to a data.frame
  wkbkOutput = cbind(wkbkOutput, Perf)                               # append it to the output object
  cols2use = c(1,rep(c(1:nExam), each = 3) + c(1,2*nExam+1,nExam+1)) # determine which columns to use
  wkbkOutput = wkbkOutput[,cols2use]                                 # reorganize the output object columns
  wkbkOutput$Cohort = Workbook$`Cohort.Year.(year.1st.entered.9th)`[match(wkbkOutput$ID,Workbook$`Local.ID.(optional)`)] # add cohorts
  mathcols = paste0("OldMath",1:6)                                                 # Names of columns to add
  wkbkOutput[,mathcols] = ""                                                       # add blank columns
  colNams = colnames(wkbkOutput)                                                   # get wkbkOutput column names
  colNams = c("Cohort", colNams[1:4], mathcols, colNams[5:(ncol(wkbkOutput) - 7)]) # reorder the column names
  wkbkOutput = wkbkOutput[,colNams]                                                # reorder the columns of wkbkOutput
  colnames(wkbkOutput)[1] = "Cohort"
  
  # Should the next line use dBtools::DFna.to.empty() ?
  for (i in 1:ncol(wkbkOutput)){wkbkOutput[,i] = as.character(wkbkOutput[,i])} # convert everything to character
  wkbkOutput[is.na(wkbkOutput)] = ""                                           # replace NA values with blanks
  write.csv(wkbkOutput, paste0(OutFolder,"PasteThisIntoTheWorkBook.csv"))      # export the file
  print("paste scores into the workbook")
} else {
  print("Workbook is fine")
}
#Note:
#     The output has to be pasted one cohort at a time
#     You will have to refill the formulas for the performance levels, but there is a macro for that.


#---------------------------------------------------------#
#### Make category score matrix from the CompareMatrix ####
#---------------------------------------------------------#

Exam2CatTable = data.frame(
  matrix(data = c("AlgOld",    "Alg",
                  "AlgCC",     "Alg",
                  "GeomOld",   "Geom",
                  "GeomCC",    "Geom",
                  "ELAOld",    "ELA",
                  "ELACC",     "ELA",
                  "Trig",      "Trig",
                  "Alg2CC",    "Trig",
                  "Bio",       "Bio",
                  "Earth",     "Earth",
                  "Phys",      "Phys",
                  "US",        "US",
                  "Chem",      "Chem",
                  "Global",    "Global",
                  "GlobTrans", "Global"), 
         ncol = 2, byrow = T),
  stringsAsFactors = F)
colnames(Exam2CatTable) = c("Exam", "Category")

Alg = VbetterMax(CompareMatrix[,"AlgOld"], CompareMatrix[,"AlgCC"])    # get the best scores across the two algebra exams
Geom = VbetterMax(CompareMatrix[,"GeomOld"], CompareMatrix[,"GeomCC"]) # get the best scores across the two geometry exams
ELA = VbetterMax(CompareMatrix[,"ELAOld"], CompareMatrix[,"ELACC"])    # get the best scores across the two ELA exams
Trig = VbetterMax(CompareMatrix[,"Trig"], CompareMatrix[,"Alg2CC"])    # get the best scores across the two Alg2 exams
Global = VbetterMax(CompareMatrix[,"Global"], CompareMatrix[,"GlobTrans"])    # get the best scores across the two Alg2 exams
CatBest = cbind(Alg, Geom, Trig, CompareMatrix[,5:9], Global, ELA)            # make a matrix of the best scores by category
# Note: It would be better if the prior line used column names instead of numbers, but that doesn't seem to work with matrices



#-------------------------------#
#### Make PowerSchool matrix ####
#-------------------------------#

names(PowerSchool)[1] = "ID"                         # change the name of the ID variable to ID
psMatrix = matrix(integer(0), n, nCat)               # set up the matrix to hold the scores
rownames(psMatrix) = studentlist                     # in the matrix, name the rows according to the student ID
colnames(psMatrix) = Categories                      # in the matrix, name the columns according to the exam name
psCategoryNames  = c("Regents_Algebra_Score", "Regents_Geometry_Score", "Regents_Algebra2/Trigonometry_Score",
                     "Regents_Living_Environment_Score", "Regents_Earth_Science_Score", "Regents_Chemistry_Score",
                     "Regents_Physics_Score", "Regents_US_History_Score", "Regents_Global_History_&_Geography_Score",
                     "Regents_Comprehensive_English_Score")
for (i in studentlist){       # for each student
  if (i %in% PowerSchool$ID){ # if student is in powerschool data, load scores into the appropriate row in the powerschool score matrix
    psMatrix[which(rownames(psMatrix) == i),] = as.integer(PowerSchool[PowerSchool$ID == i,psCategoryNames,i])
  } # /if
} # /for loop


#-------------------------------------------------------#
#### Compare CategoryOutput and PowerSchool matrices ####
#-------------------------------------------------------#

# set up the matrix with the best category scores
CatCompareMatrix = CreateCompareMatrix(studentlist, Categories, psMatrix, CatBest)

# Create list of scores that are missing from PowerSchool
# These are scores for which we have a record, but are not showing in PS
if(sum(!MbetterComp(CatCompareMatrix, psMatrix) & !is.na(CatCompareMatrix))){                    # if any best scores not in PowerSchool
  temp = which(!MbetterComp(CatCompareMatrix, psMatrix) & !is.na(CatCompareMatrix), arr.ind = T) # make matrix of locations (row and col)
  badPS = data.frame(                                                                            # make output w/ stud ID's & exam cat
    ID = as.double(rownames(temp)), 
    Categories = as.character(Categories[temp[,2]]), 
    stringsAsFactors = FALSE) 
  badPS$psScore = psMatrix[cbind(temp[,"row"],temp[,"col"])]                        # use locations to pull scores that in PowerSchool
  badPS$dbScore = CatCompareMatrix[cbind(temp[,"row"],temp[,"col"])]                # use locations to pull scores that in the Database
  badPS$First = Workbook$First.Name[match(badPS$ID,Workbook$`Local.ID.(optional)`)] # pull in the first name
  badPS$Last = Workbook$Last.Name[match(badPS$ID,Workbook$`Local.ID.(optional)`)]   # pull in the last name
  badPS$Session = ""                                                                # create a column to hold the test session name
  
  
  # This loop is supposed to add in test sessions
  for (i in 1:nrow(badPS)){
    thisrow = which(studentlist == badPS$ID[i])                                 # Determine this student's row of the dbMatrix
    examset = Exam2CatTable$Exam[Exam2CatTable$Category == badPS$Categories[i]] # Determine the exam(s) relevant to thiscategory
    thiscolumn = which(colnames(dbMatrix) %in% examset)                         # Determine the column(s) relevant to this exam
      
    if(length(thiscolumn) == 1){                                                # If the category has only one exam, 
      badPS$Session[i] = dbSessions[thisrow, thiscolumn]                        # grab that session.
    } else {                                                                    # If the category has multiple exams,
      score = badPS$dbScore[i]                                                  # grab the score.
      usethisone = which(dbMatrix[thisrow,thiscolumn] == score)                 # Grab the column(s) with that score for that student
      usethisone = usethisone[1]                                                # If score appears more than once, use first one
      badPS$Session[i] = dbSessions[thisrow,thiscolumn[usethisone]]             # Grab the session
    } # /if-else
  } # /for loop
  
  write.csv(badPS,file = paste0(OutFolder, "ScoresMissingFromPowerSchool.csv"))     # export the file
  print("Enter scores in PowerSchool")
} else {
  print("No scores to add to PowerSchool")
} #end of if-else statement
# Note: If needed, go through the ScoresMissingFromPowerSchool.csv file. 


#------------------------------------------------------------------------------------#
#### Create a list of things that need to be updated in the database/Workbook     ####
#------------------------------------------------------------------------------------#

# These are PowerSchool scores for which we have no record in the database or Workbook.sub
# Someone put them in PS, but never gave them to the data office
# CatBest is the set of best scores taken from the Workbook.sub and Regents Database
# CatCompareMatrix is the set of best scores taken from the Workbook.sub, Database, and PowerSchool

temp = !MbetterComp(CatCompareMatrix, CatBest)
if(sum(temp) > 0){                                   # if there are any best scores not in the database
  temp = which(temp, arr.ind = TRUE)                 # create a list of the locations (row and column)
  badWork = data.frame(                              # create the output object with student ID's and exam categories
    ID = as.double(rownames(temp)), 
    Categories = as.character(Categories[temp[,2]]), 
    stringsAsFactors = FALSE) 
  badWork$psScore = psMatrix[cbind(temp[,"row"],temp[,"col"])]                           # use locations to pull scores in PowerSchool
  badWork$dbScore = CatBest[cbind(temp[,"row"],temp[,"col"])]                            # use locations to pull scores in the Database
  badWork$First = Workbook$First.Name[match(badWork$ID, Workbook$`Local.ID.(optional)`)] # pull in the first name
  badWork$Last = Workbook$Last.Name[match(badWork$ID, Workbook$`Local.ID.(optional)`)]   # pull in the last name
  badWork$Session = ""                                                                   # create a column to hold the test session name
  for (i in 1:nrow(badWork)){
    thisrow = which(studentlist == badWork$ID[i])
    thiscolumn = which(substr(colnames(dbMatrix), 1, nchar(badWork$Categories[i])) == badWork$Categories[i])
    if(length(thiscolumn) == 1){
      badWork$Session[i] = dbSessions[thisrow, thiscolumn]
    } else {
      score = badWork$dbScore[i]
      usethisone = which(dbMatrix[thisrow,thiscolumn] == score)[1]
      badWork$Session[i] = dbSessions[thisrow,thiscolumn[usethisone]]
    } # /if-else
  } # /for loop
  write.csv(badWork,file = paste0(OutFolder, "ScoresInPowerSchoolButNowhereElse.csv")) # export the file
  print("There are mysterious PowerSchool scores")
} else {
  print("No mysterious PowerSchool scores found")
}



#----------------------------------------------------------------------------------------------------------#
#### Create a subset of the score database that refers to only those students who have score mismatches ####
#----------------------------------------------------------------------------------------------------------#

# Note: this part is usually unnecessary and generally unhelpful

if(exists("badWork") + exists("badPS") == 2){
  studentsToUse = unique(c(badWork$ID, badPS$ID))
} else if(exists("badWork")){
  studentsToUse = unique(c(badWork$ID))
} else if(exists("badPS")){
  studentsToUse = unique(c(badPS$ID))
} else {
  studentsToUse = NA
} # /if-else-else-else

if(!all(is.na(studentsToUse))){
  badStudents = RegentsDBraw[which(RegentsDBraw$StudentNumber %in% studentsToUse),]
  badStudents$First = Workbook$First.Name[match(badStudents$StudentNumber, Workbook$`Local.ID.(optional)`)]
  badStudents$Last = Workbook$Last.Name[match(badStudents$StudentNumber, Workbook$`Local.ID.(optional)`)]
  vars = names(badStudents)
  x = length(vars)
  badStudents = badStudents[,c(vars[x-1], vars[x], vars[1:(x-2)])]
  badStudents = DFna.to.empty(badStudents)
  write.csv(badStudents, file = paste0(OutFolder,"ScoresAndSessionsForStudentsWithScoreIssues.csv")) # export the file
  print("Look at the composite file for students with score issues")
} else {
  print("There are no issues to resolve")
} # /if-else


