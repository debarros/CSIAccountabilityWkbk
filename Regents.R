#Regents
source("functions.R")

#-----------------------------#
# Load the data from files ####
#-----------------------------#

#Get the the Output query from the Regents database (located at data drive/database project/regents.accdb)
#Paste it into the files RegentsDB.csv in the folder for this project
#note: if new exams (not just new scores) have been entered, 
#      the sql code for the Output query will need to be modified 
#      to pull that exam's scores and dates from TestScoreOutput and TestDateOutput
RegentsDBraw = read.csv("RegentsDB.csv", stringsAsFactors = FALSE) 


#Load the All Info in 1 Sheet tab from the Accountability Workbook
Workbookraw.xlsx = read.xlsx(
  xlsxFile = "J:/Accountability Spreadsheet/working copy/Green Tech Cohort Data Collection Workbook.xlsx", 
  sheet = "All info in 1 sheet",startRow = 2)
Workbookraw = Workbookraw.xlsx[!is.na(Workbookraw.xlsx$`Local.ID.(optional)`),-c(1,2,3)]



#see How to Export All Regents Scores from PowerSchool (Google Drive > Instructions > PowerSchool)
#Put the output into the file PowerSchoolRegents.csv in the folder for this project
PowerSchoolraw = read.csv("PowerSchoolRegents.csv", stringsAsFactors = FALSE) 

#save the raw versions in case the raw data is needed later
RegentsDB = RegentsDBraw
Workbook = Workbookraw
PowerSchool = PowerSchoolraw


#---------------------------------#
# Set the exams and categories ####
#---------------------------------#

#if new exams have been introduced, you'll need to add them here
Exams = c("ELAOld","AlgOld","GeomOld","Trig","Bio","Earth","Chem","Phys","US","Global","ELACC","AlgCC","GeomCC", "Alg2CC")
Categories = c("Alg","Geom","Trig","Bio","Earth","Chem","Phys","US","Global","ELA")



#--------------------------------------------------#
# Make Workbook score matrix and session matrix ####
#--------------------------------------------------#

str(Workbook) #take a look at what is in the Workbook object

#remove blank rows and unnecessary columns
# this line should keep the student ID and all of the regents scaled scores, performance levels, and exam year/terms
Workbook = Workbook[, c(2, 37:39,46:84)]  

names(Workbook)[1] = "ID"             #change the first column to be named ID
studentlist = as.double(Workbook[,1]) #this needs to be a double precision because some ID's are very long
n = length(studentlist)               #this is the number of students
wkbkMatrix = matrix(nrow = n, ncol = length(Exams), data = integer(0))     #set up the matrix to hold scores from the workbook
wkbkSessions = matrix(nrow = n, ncol = length(Exams), data = character(0)) #set up the matrix to hold sessions from the workbook
rownames(wkbkMatrix) = studentlist   #in the matrices, name the rows according to the student ID
rownames(wkbkSessions) = studentlist
colnames(wkbkMatrix) = Exams         #in the matrices, name the columns according to the exam name
colnames(wkbkSessions) = Exams

# Load in the actual scores and sessions ##
cols = 3*(1:length(Exams))-1  #the scores are in columns 2, 5, 8, etc.
for (i in studentlist){
  wkbkMatrix[which(rownames(wkbkMatrix) == i),] = as.integer(Workbook[which(Workbook$ID == i),cols])
  wkbkSessions[which(rownames(wkbkSessions) == i),] = as.character(Workbook[which(Workbook$ID == i),cols+2])
} #end of for loop



#----------------------------------------------------------#
# Make Regents Database score matrix and session matrix ####
#----------------------------------------------------------#

names(RegentsDB)[1] = "ID"                                               #change the first column to be named ID
dbMatrix = matrix(nrow = n, ncol = length(Exams), data = integer(0))     #set up the matrix to hold scores from the database
dbSessions = matrix(nrow = n, ncol = length(Exams), data = character(0)) #set up the matrix to hold sessions from the database

rownames(dbMatrix) = studentlist    #in the matrices, name the rows according to the student ID
rownames(dbSessions) = studentlist
colnames(dbMatrix) = Exams          #in the matrices, name the columns according to the exam name
colnames(dbSessions) = Exams

# load in the actual scores and sessions
for (i in studentlist){
  dbMatrix[which(rownames(dbMatrix) == i),] = as.integer(RegentsDB[which(RegentsDB$ID == i),2*(1:length(Exams))])
  dbSessions[which(rownames(dbSessions) == i),] = as.character(RegentsDB[which(RegentsDB$ID == i),(2*(1:length(Exams))+1)])
} #end of for loop



#------------------------------------------#
# Compare Regents Database and Workbook ####
#------------------------------------------#

CompareMatrix = matrix(nrow = n, ncol = length(Exams), data = integer(0))   #set up a matrix that will hold the best scores
rownames(CompareMatrix) = studentlist              #in the matrix, name the rows according to the student ID
colnames(CompareMatrix) = Exams                    #in the matrix, name the columns according to the exam name
CompArray = abind(dbMatrix,wkbkMatrix,along = 3)   #bind the two matrices of scores into a 3-d array
CompareMatrix = MbetterMax(CompArray)              #for each student/exam intersection, pick the better of the two scores


#Create list things that need to be updated in the database
if(sum(!MbetterComp(CompareMatrix, dbMatrix) & !is.na(CompareMatrix))){                    #if there are any best scores not in the database
  temp = which(!MbetterComp(CompareMatrix, dbMatrix) & !is.na(CompareMatrix), arr.ind = T) #create a list of the locations (row and column)
  badDB = data.frame(ID = rownames(temp), Exam = Exams[temp[,2]])                          #create a list of the... something?
  write.csv(badDB,file = "ScoresMissingFromDataBase.csv")                                  #export the file
}

#Note:
#     Take a look at the ScoresMissingFromDatabase.csv file
#     Those are instances in which a student has a score in the workbook but not in the database


#Create output to be pasted into the workbook
wkbkOutput = data.frame(ID = rownames(CompareMatrix))   #set up the output object
temp = CompareMatrix                                    #create a temporary version of the best scores matrix
rownames(temp) = NULL                                   #get rid of the rownames in temp so it can be merged with the output object
wkbkOutput = cbind(wkbkOutput, temp)                    #merge the best scores with the output object, so that it has student ID's
rownames(wkbkMatrix) = NULL                             #get rid of row names from everything
rownames(dbMatrix) = NULL
rownames(wkbkSessions) = NULL
rownames(dbMatrix) = NULL

for (i in 1:length(Exams)){                                         #for each exam,
  wkbkOutput[,length(Exams)+1+i] = NA                               #add a column for it in the output object
  grabRows = which(wkbkOutput[,1+i] == wkbkMatrix[,i])              #find which scores are consistent with the workbook
  wkbkOutput[grabRows,length(Exams)+1+i] = wkbkSessions[grabRows,i] #get the sessions from the workbook and insert them in the output object
  grabRows = which(wkbkOutput[,1+i] == dbMatrix[,i])                               #find which scores are consistent with the database
  wkbkOutput[grabRows,length(Exams)+1+i] = dbSessions[grabRows,i]                  #get the sessions from the database and insert them in the output object
  names(wkbkOutput)[length(Exams)+1+i] = paste0(names(wkbkOutput)[1+i]," Session") #name the new column by the test name plus the word session
} #end of for loop

Perf = matrix(ncol = length(Exams), nrow = n, data = NA)  #create empty variables to serve as the performance index columns
Perf = data.frame(Perf)                                   #convert it to a data.frame
wkbkOutput = cbind(wkbkOutput, Perf)                      #append it to the output object
wkbkOutput = wkbkOutput[,c(1,rep(c(1:length(Exams)), each = 3) + c(1,2*length(Exams)+1,length(Exams)+1))]   #reorganize the output object columns
wkbkOutput = cbind.data.frame(Workbookraw$Cohort.Year..year.1st.entered.9th., wkbkOutput)


for (i in 1:ncol(wkbkOutput)){wkbkOutput[,i] = as.character(wkbkOutput[,i])} #convert everything to character
wkbkOutput[is.na(wkbkOutput)] = ""                                           #replace NA values with blanks
write.csv(wkbkOutput,file = "PasteThisIntoTheWorkBook.csv")                  #export the file

#Note:
#     The output is not perfectly ordered for easy pasting
#     You will have to refill the formulas for the performance levels


#------------------------------------------------------#
# Make category score matrix from the CompareMatrix ####
#------------------------------------------------------#

Alg = VbetterMax(CompareMatrix[,"AlgOld"], CompareMatrix[,"AlgCC"])     #get the best scores across the two algebra exams
Geom = VbetterMax(CompareMatrix[,"GeomOld"], CompareMatrix[,"GeomCC"])  #get the best scores across the two geometry exams
ELA = VbetterMax(CompareMatrix[,"ELAOld"], CompareMatrix[,"ELACC"])     #get the best scores across the two ELA exams
Trig = VbetterMax(CompareMatrix[,"Trig"], CompareMatrix[,"Alg2CC"])     #get the best scores across the two Alg2 exams
CatBest = cbind(Alg, Geom, Trig, CompareMatrix[,5:10], ELA)             #make a matrix of the best scores by category



#----------------------------#
# Make PowerSchool matrix ####
#----------------------------#

names(PowerSchool)[1] = "ID"                      #change the name of the ID variable to ID
psMatrix = matrix(                                #set up the matrix to hold the scores
  nrow = length(studentlist), 
  ncol = length(Categories), 
  data = integer(0))   
rownames(psMatrix) = studentlist                  #in the matrix, name the rows according to the student ID
colnames(psMatrix) = Categories                   #in the matrix, name the columns according to the exam name
for (i in studentlist){                           #for each student
  if (i %in% PowerSchool$ID){                     #if that student appears in the powerschool data,
    psMatrix[which(rownames(psMatrix) == i),] =   #load their scores into the appropriate row in the powerschool score matrix
      as.integer(PowerSchool[PowerSchool$ID == i,c(15, 17, 19, 18, 8, 16, 21, 14, 7, 20),i])
  } #end if
} #end of for loop



#----------------------------------------------------#
# Compare CategoryOutput and PowerSchool matrices ####
#----------------------------------------------------#

CatCompareMatrix = matrix(                         #set up the matrix to hold the best category scores
  nrow = n, 
  ncol = length(Categories), 
  data = integer(0))
rownames(CatCompareMatrix) = studentlist           #in the matrix, name the rows according to the student ID
colnames(CatCompareMatrix) = Categories            #in the matrix, name the columns according to the exam name
CatCompArray = abind(psMatrix,CatBest,along = 3)   #bind the two matrices of scores into a 3-d array
CatCompareMatrix = MbetterMax(CatCompArray)        #for each student/exam intersection, pick the better of the two scores


#Create list of scores that are missing from PowerSchool
#These are scores for which we have a record, but are not showing in PS
if(sum(!MbetterComp(CatCompareMatrix, psMatrix) & !is.na(CatCompareMatrix))){                       #if there are any best scores not in PowerSchool
  temp = which(!MbetterComp(CatCompareMatrix, psMatrix) & !is.na(CatCompareMatrix), arr.ind = TRUE) #create a list of the locations (row and column)
  badPS = data.frame(                                                                               #create the output object with student ID's and exam categories
    ID = as.double(rownames(temp)), 
    Categories = as.character(Categories[temp[,2]]), 
    stringsAsFactors = FALSE) 
  badPS$psScore = psMatrix[cbind(temp[,"row"],temp[,"col"])]                              #use the locations to pull the scores that are in PowerSchool
  badPS$dbScore = CatCompareMatrix[cbind(temp[,"row"],temp[,"col"])]                      #use the locations to pull the scores that are in the Database
  badPS$First = NA                                                                        #create a variable to hold the first name
  badPS$Last = NA                                                                         #create a variable to hold the last name
  
  for(i in studentlist){                                                                  #for each student
    badPS$First[which(badPS$ID == i)] = Workbookraw$First.Name[which(Workbookraw$Local.ID..optional. == i)]  #pull in the first name
    badPS$Last[which(badPS$ID == i)] = Workbookraw$Last.Name[which(Workbookraw$Local.ID..optional. == i)]    #pull in the last name
  } #end of for loop
  
  badPS$Session = ""
  
  for (i in 1:nrow(badPS)){
    thisrow = which(studentlist == badPS$ID[i])
    thiscolumn = which(substr(colnames(dbMatrix),start = 1, stop = nchar(badPS$Categories[i])) == badPS$Categories[i])
    if(length(thiscolumn) == 1){
      badPS$Session[i] = dbSessions[thisrow, thiscolumn]
    } else {score = badPS$dbScore[i]
    usethisone = which(dbMatrix[thisrow,thiscolumn] == score)[1]
    badPS$Session[i] = dbSessions[thisrow,thiscolumn[usethisone]]
    } #end of else
  } #end of for loop
  
  write.csv(badPS,file = "ScoresMissingFromPowerSchool.csv") #export the file
} #end of if statement

# Note: 
#      Go through the ScoresMissingFromPowerSchool.csv file.
#      Note that it doesn't catch everything.
#      I'm not sure why it doesn't.  



#----------------------------------------------------------------------------#
#Create a list of things that need to be updated in the database/workbook ####
#----------------------------------------------------------------------------#

#These are PowerSchool scores for which we have no record in the database or workbook
#Someone put them in PS, but never gave them to the data office
#CatBest is the set of best scores taken from the Workbook and Regents Database
#CatCompareMatrix is the set of best scores taken from the Workbook, Database, and PowerSchool

temp = !MbetterComp(CatCompareMatrix, CatBest)
if(sum(temp)>0){          #if there are any best scores not in the database
  temp = which(temp, arr.ind = TRUE)  #create a list of the locations (row and column)
  badWork = data.frame(                                                                 #create the output object with student ID's and exam categories
    ID = as.double(rownames(temp)), 
    Categories = as.character(Categories[temp[,2]]),
    stringsAsFactors = FALSE) 
  badWork$psScore = psMatrix[cbind(temp[,"row"],temp[,"col"])]                          #use the locations to pull the scores that are in PowerSchool
  badWork$dbScore = CatBest[cbind(temp[,"row"],temp[,"col"])]                           #use the locations to pull the scores that are in the Database
  badWork$First = NA                                                                    #create a variable to hold the first name
  badWork$Last = NA                                                                     #create a variable to hold the last name
  
  for(i in studentlist){                                                                                         #for each student
    badWork$First[which(badWork$ID == i)] = Workbookraw$First.Name[which(Workbookraw$Local.ID..optional. == i)]  #pull in the first name
    badWork$Last[which(badWork$ID == i)] = Workbookraw$Last.Name[which(Workbookraw$Local.ID..optional. == i)]    #pull in the last name
  } #end of for loop
  
  badWork$Session = ""
  for (i in 1:nrow(badWork)){
    thisrow = which(studentlist == badWork$ID[i])
    thiscolumn = which(substr(colnames(dbMatrix),start = 1, stop = nchar(badWork$Categories[i])) == badWork$Categories[i])
    if(length(thiscolumn) == 1){
      badWork$Session[i] = dbSessions[thisrow, thiscolumn]
    } else {score = badWork$dbScore[i]
    usethisone = which(dbMatrix[thisrow,thiscolumn] == score)[1]
    badWork$Session[i] = dbSessions[thisrow,thiscolumn[usethisone]]
    }
  } #end of for loop
  
  write.csv(badWork,file = "ScoresInPowerSchoolButNowhereElse.csv") #export the file
}




#Create a subset of the score database that refers to only those students who have score mismatches
studentsToUse = unique(c(badWork$ID, badPS$ID))
badStudents = RegentsDBraw[which(RegentsDBraw$StudentNumber %in% studentsToUse),]
badStudents$First = NA
badStudents$Last = NA

for(i in studentsToUse){
  badStudents$First[which(badStudents$StudentNumber == i)] = Workbookraw$First.Name[which(Workbookraw$Local.ID..optional. == i)]
  badStudents$Last[which(badStudents$StudentNumber == i)] = Workbookraw$Last.Name[which(Workbookraw$Local.ID..optional. == i)]
} #end of for loop

vars = names(badStudents)
x = length(vars)
badStudents = badStudents[,c(vars[x-1], vars[x], vars[1:(x-2)])]

write.csv(badStudents, file = "ScoresAndSessionsForStudentsWithScoreIssues.csv") #export the file