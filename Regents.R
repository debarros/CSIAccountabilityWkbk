# Regents.R
# This is for reconciling regents scores in the regents workbook, powerschool, and the accountability workbook
# Note: Run "Regents Workbook.R" first.

# Save the raw versions in case the raw data is needed later (e.g., restarting this script without rerunning MainScript)
R.bs = Regents.bestScores
R.bs.ne = Regents.bestScores.NE
R.bc = Regents.bestCategory
R.bc.ne = Regents.bestCategory.NE
Workbook.sub = Workbook
PS = powerschoolraw

#------------------------------------#
#### Set the exams and categories ####
#------------------------------------#

Exams = testLookup$Code
nExam = length(Exams)
Categories = unique(testLookup$Category)
nCat = length(Categories)


#---------------------------------------------------------#
#### Make Workbook.sub score matrix and session matrix ####
#---------------------------------------------------------#

# Remove blank rows and unnecessary columns
# this line should keep the student ID and all of the regents scaled scores, performance levels, and exam year/terms
PSexamnames = unique(testLookup$PowerSchool)

column_names = CharCartProd(x = testLookup$Workbook, y = c("Scaled.Score", "Exam.Year.and.Term"), groupBy = 2)
column_names = c("Local.ID.(optional)", column_names)

if(!(all(column_names %in% colnames(Workbook.sub)))){
  print("There's a problem with the column names")
}

Workbook.sub = Workbook.sub[,column_names]     # subset the workbook to the relevant columns
Workbook.sub$USF.HistoryExam.Year.and.Term = as.character(Workbook.sub$USF.HistoryExam.Year.and.Term)
colnames(Workbook.sub)[1] = "ID"               # change the first column to be named ID
studentlist = as.double(Workbook.sub$ID)       # this needs to be a double precision because some ID's are very long
n = length(studentlist)                        # this is the number of students

# Set up matrices to hold scores and sessions from the workbook, using student ID's as row names and exam names as column names
wkbkMatrix = matrix(numeric(0), n, nExam, F, list(studentlist, Exams))
wkbkSessions = matrix(character(0), n, nExam, F, list(studentlist, Exams))
Workbook.sub[which(Workbook.sub == "E", arr.ind = T)] = 64.9


# Load in the actual scores and sessions
cols = 2*(1:nExam)  #the scores are in columns 2, 4, 6, etc.
for (i in studentlist){
  wkbkMatrix[rownames(wkbkMatrix) == i,] = as.numeric(Workbook.sub[Workbook.sub$ID == i,cols])
  wkbkSessions[which(rownames(wkbkSessions) == i),] = as.character(Workbook.sub[which(Workbook.sub$ID == i),cols+1])
} # /for


#-------------------------------------------------------------#
#### Make Regents Database score matrix and session matrix ####
#-------------------------------------------------------------#

R.bs.ne$ExamCode = testLookup$Code[match(R.bs.ne$Exam, testLookup$Database)]
R.bs.ne = R.bs.ne[!is.na(R.bs.ne$ExamCode),]
colnames(R.bs.ne)[1] = "ID"

# Set up the matrices to hold scores and sessions from the database with student IDs and rownames and exam names as column names
dbMatrix = matrix(integer(0), n, nExam, F, list(studentlist, Exams))
dbSessions = matrix(character(0), n, nExam, F, list(studentlist, Exams))

# load in the actual scores and sessions
for(thisStudent in studentlist){
  for(thisExam in Exams){
    thisScoreValue = R.bs.ne$ScoreValue[R.bs.ne$ExamCode == thisExam & R.bs.ne$ID == thisStudent]
    if(length(thisScoreValue) != 0){
      thisSession = R.bs.ne$SessionName[R.bs.ne$ExamCode == thisExam & R.bs.ne$ID == thisStudent]
      thisRow = which(rownames(dbMatrix) == thisStudent) 
      thisColumn = which(colnames(dbMatrix) == thisExam)
      dbMatrix[thisRow,thisColumn] = thisScoreValue
      dbSessions[thisRow,thisColumn] = thisSession
    }
  }
} # /for
# Note: if you get warnings here, there is likely a problem with the order of the columns in the database output


# Are there other problems?  Check it out:
#  studentlist[!(studentlist %in% R.bs$ID)]
#  R.bs$ID[!(R.bs$ID %in% studentlist)]
#  View(Workbookraw.xlsx[Workbookraw.xlsx$`Local.ID.(optional)` %in% R.bs$ID[!(R.bs$ID %in% studentlist)],])


#---------------------------------------------#
#### Compare Regents Database and Workbook ####
#---------------------------------------------#

# Create a Compare Matrix with  the best scores
CompMat = CreateCompareMatrix(studentlist, Exams, dbMatrix, wkbkMatrix)

if(!(identical(colnames(dbMatrix), colnames(wkbkMatrix)) & identical(colnames(dbMatrix), colnames(CompMat)))){
  message("ERROR!  There is a problem the order of the columns in the matrices.")
}

# Create a list of things that need to be updated in the database
if(sum(!MbetterComp(CompMat, dbMatrix) & !is.na(CompMat))){                    # if any best scores not in the database
  temp = which(!MbetterComp(CompMat, dbMatrix) & !is.na(CompMat), arr.ind = T) # create matrix of locations (row & col)
  badDB = data.frame(ID = rownames(temp), Exam = Exams[temp[,2]])              # create a list of the... something?
  write.csv(badDB,file = paste0(OutFolder,"ScoresMissingFromDataBase.csv"))    # export the file
  print("Resolve the scores missing from the Database")
} else {
  print("No scores missing from the Database")
}
# Note:
#     Take a look at the ScoresMissingFromDatabase.csv file
#     Those are instances in which a student has a score in the workbook but not in the database


# Check if there are any best scores not in the workbook, and if so, create output to be pasted into the Workbook
if(sum(!MbetterComp(CompMat, wkbkMatrix) & !is.na(CompMat)) > 0){ 
  print(which(!MbetterComp(CompMat, wkbkMatrix) & !is.na(CompMat), arr.ind = T))
  
  wkbkOutput = data.frame(ID = rownames(CompMat)) # set up the output object
  temp = CompMat                                  # create a temporary version of the best scores matrix
  rownames(temp) = NULL                           # get rid of the rownames in temp so it can be merged with the output object
  wkbkOutput = cbind(wkbkOutput, temp)            # merge the best scores with the output object, so that it has student ID's
  rownames(wkbkMatrix) = NULL                     # get rid of row names from everything
  rownames(dbMatrix) = NULL
  rownames(wkbkSessions) = NULL
  rownames(dbMatrix) = NULL
  
  for (i in 1:nExam){                                              # for each exam,
    wkbkOutput[,nExam+1+i] = NA                                    # add a column for it in the output object
    grabRows = which(wkbkOutput[,1+i] == wkbkMatrix[,i])           # find which scores are consistent with the Workbook.sub
    wkbkOutput[grabRows,nExam+1+i] = wkbkSessions[grabRows,i]      # get Workbook sessions and insert in output object
    grabRows = which(wkbkOutput[,1+i] == dbMatrix[,i])             # find which scores are consistent with the database
    wkbkOutput[grabRows,nExam+1+i] = dbSessions[grabRows,i]        # get sessions from database and insert in output object
    names(wkbkOutput)[nExam+1+i] = paste0(names(wkbkOutput)[1+i],  # name new column by test name plus the word session
                                          " Session") 
  } #end of for loop
  
  
  # Add old math exam columns
  colNams = colnames(wkbkOutput)                                                   # get wkbkOutput column names
  mathcols = paste0("OldMath",1:2)                                                 # Names of columns to add
  colNams = c(colNams[1:2], mathcols, colNams[3:length(colNams)])                  # reorder the column names
  wkbkOutput[,mathcols] = ""                                                       # add blank columns
  wkbkOutput = wkbkOutput[,colNams]                                                # reorder the columns of wkbkOutput
  mathcolsSession = paste0(mathcols, " Session")
  wkbkOutput[,mathcolsSession] = ""                                                # add blank columns
  colNams = colnames(wkbkOutput)
  
  # Add columns for the exemptions
  scoreColNames = colNams[2:((length(colNams) + 1)/2)]
  SessionColNames = paste0(scoreColNames, " Session")
  exemptioncolnames = paste0(scoreColNames, "_Exemption")
  wkbkOutput[,exemptioncolnames] = ""                                              # add blank columns
  
  # Assemble the output with the columns in the correct order
  allColNams = "ID"
  for(thisCol in 1:length(scoreColNames)){
    allColNams = c(allColNams, scoreColNames[thisCol], SessionColNames[thisCol], exemptioncolnames[thisCol])
  }
  wkbkOutput = wkbkOutput[,allColNams]
  
  
  
  # Load exemptions
  R.bs.exemptions = R.bs[R.bs$Score == "E",]
  rownames(R.bs.exemptions) = NULL
  R.bs.exemptions$wkbkExportColumn = testLookup$Code[match(R.bs.exemptions$Exam, testLookup$Database)]
  if(sum(is.na(R.bs.exemptions$wkbkExportColumn)) > 0){
    print("There's a problem with the lookup of testlookup$Database and R.bs.exemptions$Exam")
  }
  
  
  for(thisRow in 1:nrow(R.bs.exemptions)){
    outColumn = paste0(R.bs.exemptions$wkbkExportColumn[thisRow],"_Exemption")
    outRow = wkbkOutput$ID == R.bs.exemptions$StudentNumber[thisRow]
    wkbkOutput[outRow, outColumn] = R.bs.exemptions$SessionName[thisRow]
  }
  
  
  
  # Write the output
  for (i in 1:ncol(wkbkOutput)){wkbkOutput[,i] = as.character(wkbkOutput[,i])} # convert everything to character
  wkbkOutput = DFna.to.empty(wkbkOutput)                                       # replace NA values with blanks
  write.csv(wkbkOutput, paste0(OutFolder,"PasteThisIntoTheWorkBook.csv"))      # export the file
  print("paste scores into the workbook")
} else {
  print("Workbook is fine")
}
# Note: You can paste all scores and dates at once.



#---------------------------------------------------------#
#### Make category score matrix from the CompMat ####
#---------------------------------------------------------#

R.bc.ne = R.bc.ne[R.bc.ne$Category %in% testLookup$Category,]
colnames(R.bc.ne)[1] = "ID"
CatBest = matrix(data = numeric(0), n, nCat, F, list(studentlist, Categories))
for(thisStudent in studentlist){
  for(thisCategory in Categories){
    thisScoreValue = R.bc.ne$ScoreValue[R.bc.ne$Category == thisCategory & R.bc.ne$ID == thisStudent]
    if(length(thisScoreValue) != 0){
      thisRow = which(rownames(CatBest) == thisStudent) 
      thisColumn = which(colnames(CatBest) == thisCategory)
      CatBest[thisRow,thisColumn] = thisScoreValue
    }
  }
} # /for



#-------------------------------#
#### Make PowerSchool matrix ####
#-------------------------------#

colnames(PS)[1] = "ID"                                              # change the name of the ID variable to ID
psMatrix = matrix(integer(0), n, nCat)                              # set up the matrix to hold the scores
rownames(psMatrix) = studentlist                                    # in the matrix, name the rows according to the student ID
colnames(psMatrix) = Categories                                     # in the matrix, name the columns according to the exam name
psCategoryNames = testLookup$PowerSchool[match(Categories, testLookup$Category)] # match category names to PS field names
PS.limited = PS[,c("ID",psCategoryNames)]
PS.limited[which(PS.limited == "E", arr.ind = T)] = "64.9"
str(PS.limited)

sort(unique(unlist(PS.limited[,psCategoryNames])))

for (thisStu in studentlist){ # for each student
  if (thisStu %in% PS$ID){    # if student is in powerschool data, load scores into the appropriate row in the powerschool score matrix
    psMatrix[rownames(psMatrix) == thisStu,] = as.numeric(PS.limited[PS.limited$ID == thisStu,psCategoryNames])
  } # /if
} # /for loop



#-------------------------------------------------------#
#### Compare CategoryOutput and PowerSchool matrices ####
#-------------------------------------------------------#

# set up the matrix with the best category scores
CatCompMat = CreateCompareMatrix(studentlist, Categories, psMatrix, CatBest)


psMatrix[rownames(psMatrix) == "192011812",]
CatBest[rownames(CatBest) == "192011812",]
CatCompMat[rownames(CatCompMat) == "192011812",]

# Create list of scores that are missing from PowerSchool
# These are scores for which we have a record, but are not showing in PS
if(sum(!MbetterComp(CatCompMat, psMatrix) & !is.na(CatCompMat))){                    # if any best scores not in PowerSchool
  temp = which(!MbetterComp(CatCompMat, psMatrix) & !is.na(CatCompMat), arr.ind = T) # make matrix of locations (row and col)
  badPS = data.frame(                                                                # make output w/ stud ID's & exam cat
    ID = as.double(rownames(temp)), 
    Categories = as.character(Categories[temp[,2]]), 
    stringsAsFactors = FALSE) 
  badPS$psScore = psMatrix[cbind(temp[,"row"],temp[,"col"])]                        # pull scores in PowerSchool
  badPS$dbScore = CatCompMat[cbind(temp[,"row"],temp[,"col"])]                      # pull scores that in the Database
  badPS$First = Workbook$First.Name[match(badPS$ID,Workbook$`Local.ID.(optional)`)] # pull in the first name
  badPS$Last = Workbook$Last.Name[match(badPS$ID,Workbook$`Local.ID.(optional)`)]   # pull in the last name
  badPS$Session = ""                                                                # create column to hold test session name
  
  
  # This loop is supposed to add in test sessions
  for (i in 1:nrow(badPS)){
    thisrow = which(studentlist == badPS$ID[i])                                 # Determine this student's row of the dbMatrix
    examset = Exam2CatTable$Exam[Exam2CatTable$Category == badPS$Categories[i]] # Determine the exam(s) relevant to thiscategory
    thiscolumn = which(colnames(dbMatrix) %in% examset)                         # Determine the column(s) relevant to this exam
    
    if(length(thiscolumn) == 1){                                                # If the category has only one exam, 
      badPS$Session[i] = dbSessions[thisrow, thiscolumn]                        # grab that session.
    } else {                                                                    # If the category has multiple exams,
      score = badPS$dbScore[i]                                                  # grab the score.
      usethisone = which(dbMatrix[thisrow,thiscolumn] == score)                 # Grab the col(s) w/ that score for that student
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
# CatCompMat is the set of best scores taken from the Workbook.sub, Database, and PowerSchool

temp = !MbetterComp(CatCompMat, CatBest)
if(sum(temp) > 0){                                   # if there are any best scores not in the database
  temp = which(temp, arr.ind = TRUE)                 # create a list of the locations (row and column)
  badWork = data.frame(                              # create the output object with student ID's and exam categories
    ID = as.double(rownames(temp)), 
    Categories = as.character(Categories[temp[,2]]), 
    stringsAsFactors = FALSE) 
  badWork$psScore = psMatrix[cbind(temp[,"row"],temp[,"col"])]                           # pull scores in PowerSchool
  badWork$dbScore = CatBest[cbind(temp[,"row"],temp[,"col"])]                            # pull scores in the Database
  badWork$First = Workbook$First.Name[match(badWork$ID, Workbook$`Local.ID.(optional)`)] # pull in the first name
  badWork$Last = Workbook$Last.Name[match(badWork$ID, Workbook$`Local.ID.(optional)`)]   # pull in the last name
  badWork$Session = ""                                                                   # create test session name column
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
  badStudents = R.bsraw[which(R.bsraw$StudentNumber %in% studentsToUse),]
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


