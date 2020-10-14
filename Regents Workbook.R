# RegentsWorkbook.R
# This organizes the information in the regents workbook


#----------------------------------#
#### Check, clean, and organize ####
#----------------------------------#

# Check for any bizarre regents scores
if(nrow(Regents.scores[nchar(Regents.scores$Score) > 3,]) > 0){
  print("These scores don't make sense:")
  print(Regents.scores[nchar(Regents.scores$Score) > 3,])
}

# Check the data
if(any(is.na(Regents.scores$StudentNumber))){
  print("There are missing student numbers")
}
if(any(is.na(Regents.scores$Exam))){
  print("There are missing exam names")
}
if(any(is.na(Regents.scores$SessionName))){
  print("There are missing session names")
}
if(any(is.na(Regents.scores$Location))){
  print("There are missing locations")
}
if(any(is.na(Regents.scores$Score))){
  print("There are missing scores")
}


setdiff(Regents.scores$Exam, Regents.exams$Exam)
setdiff(Regents.scores$SessionName, Regents.sessions$SessionName)
setdiff(Regents.scores$Location, Regents.locations$Location)
setdiff(Regents.exams$Category, Regents.examCategories$Category)


# Add categories, category priorities, and session priorities to the scores table
Regents.scores$Category = Regents.exams$Category[match(Regents.scores$Exam, Regents.exams$Exam)]
Regents.scores$CategoryPriority = Regents.exams$CategoryPriority[match(Regents.scores$Exam, Regents.exams$Exam)]
Regents.scores$SessionPriority = Regents.sessions$SessionPriority[match(Regents.scores$SessionName, Regents.sessions$SessionName)]

if(sum(sum(is.na(Regents.scores$Category)), sum(is.na(Regents.scores$CategoryPriority)), sum(is.na(Regents.scores$SessionPriority))) > 0){
  print("something went wrong")
}



# Set up a table of scores without exemptions
Regents.scores.NE = Regents.scores[Regents.scores$Score != "E",]
Regents.scores.NE$ScoreValue = as.numeric(Regents.scores.NE$Score)
rownames(Regents.scores.NE) = NULL

if(any(is.na(Regents.scores.NE$ScoreValue))){
  print("something went wrong")
}


# Set score values in order to interpret exempted scores
Regents.scores$ScoreValue = Regents.scores$Score
Regents.scores$ScoreValue[Regents.scores$Score == "E"] = "64.9"
Regents.scores$ScoreValue = as.numeric(Regents.scores$ScoreValue)

if(any(is.na(Regents.scores$ScoreValue))){
  print("something went wrong")
}



#------------------------------------------------------------#
#### Create the table of best exam scores (no exemptions) ####
#------------------------------------------------------------#

Regents.bestScores.NE = Regents.scores.NE
Regents.bestScores.NE$isBest = F
Regents.bestScores.NE$StudentExam = paste0(Regents.bestScores.NE$StudentNumber, "-", Regents.bestScores.NE$Exam)
for(i in 1:nrow(Regents.bestScores.NE)){
  thisScore = Regents.bestScores.NE$ScoreValue[i]
  thisStudentExam = Regents.bestScores.NE$StudentExam[i]
  allTheseScores = Regents.bestScores.NE$ScoreValue[Regents.bestScores.NE$StudentExam == thisStudentExam]
  maxTheseScores = max(allTheseScores)
  if(thisScore == maxTheseScores){
    Regents.bestScores.NE$isBest[i] = T
  }
}
Regents.bestScores.NE = Regents.bestScores.NE[Regents.bestScores.NE$isBest,]
rownames(Regents.bestScores.NE) = NULL
Regents.bestScores.NE = Regents.bestScores.NE[order(Regents.bestScores.NE$SessionPriority),]
Regents.bestScores.NE = Regents.bestScores.NE[!duplicated(Regents.bestScores.NE$StudentExam),]




#--------------------------------------------------------------#
#### Create a table of best category scores (no exemptions) ####
#--------------------------------------------------------------#

Regents.bestCategory.NE = Regents.bestScores.NE
Regents.bestCategory.NE$isBest = F
Regents.bestCategory.NE$StudentCategory = paste0(Regents.bestCategory.NE$StudentNumber, Regents.bestCategory.NE$Category)
for(i in 1:nrow(Regents.bestCategory.NE)){
  thisScore = Regents.bestCategory.NE$ScoreValue[i]
  thisStudentCategory = Regents.bestCategory.NE$StudentCategory[i]
  allTheseScores = Regents.bestCategory.NE$ScoreValue[Regents.bestCategory.NE$StudentCategory == thisStudentCategory]
  maxTheseScores = max(allTheseScores)
  if(thisScore == maxTheseScores){
    Regents.bestCategory.NE$isBest[i] = T
  }
}
Regents.bestCategory.NE = Regents.bestCategory.NE[Regents.bestCategory.NE$isBest,]
rownames(Regents.bestCategory.NE) = NULL
Regents.bestCategory.NE = Regents.bestCategory.NE[order(Regents.bestCategory.NE$CategoryPriority),]
Regents.bestCategory.NE = Regents.bestCategory.NE[!duplicated(Regents.bestCategory.NE$StudentCategory),]






#-----------------------------------------------------------------#
#### Create the table of best exam scores including exemptions ####
#-----------------------------------------------------------------#

Regents.bestScores = Regents.scores
Regents.bestScores$isBest = F
Regents.bestScores$StudentExam = paste0(Regents.bestScores$StudentNumber, "-", Regents.bestScores$Exam)
for(i in 1:nrow(Regents.bestScores)){
  thisScore = Regents.bestScores$ScoreValue[i]
  thisStudentExam = Regents.bestScores$StudentExam[i]
  allTheseScores = Regents.bestScores$ScoreValue[Regents.bestScores$StudentExam == thisStudentExam]
  maxTheseScores = max(allTheseScores)
  if(thisScore == maxTheseScores){
    Regents.bestScores$isBest[i] = T
  }
}
Regents.bestScores = Regents.bestScores[Regents.bestScores$isBest,]
rownames(Regents.bestScores) = NULL
Regents.bestScores = Regents.bestScores[order(Regents.bestScores$SessionPriority),]
Regents.bestScores = Regents.bestScores[!duplicated(Regents.bestScores$StudentExam),]



#-------------------------------------------------------------------#
#### Create a table of best category scores including exemptions ####
#-------------------------------------------------------------------#

Regents.bestCategory = Regents.bestScores
Regents.bestCategory$isBest = F
Regents.bestCategory$StudentCategory = paste0(Regents.bestCategory$StudentNumber, Regents.bestCategory$Category)
for(i in 1:nrow(Regents.bestCategory)){
  thisScore = Regents.bestCategory$ScoreValue[i]
  thisStudentCategory = Regents.bestCategory$StudentCategory[i]
  allTheseScores = Regents.bestCategory$ScoreValue[Regents.bestCategory$StudentCategory == thisStudentCategory]
  maxTheseScores = max(allTheseScores)
  if(thisScore == maxTheseScores){
    Regents.bestCategory$isBest[i] = T
  }
}
Regents.bestCategory = Regents.bestCategory[Regents.bestCategory$isBest,]
rownames(Regents.bestCategory) = NULL
Regents.bestCategory = Regents.bestCategory[order(Regents.bestCategory$CategoryPriority),]
Regents.bestCategory = Regents.bestCategory[!duplicated(Regents.bestCategory$StudentCategory),]

