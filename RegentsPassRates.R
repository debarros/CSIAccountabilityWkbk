# RegentsPassRates.R

# This script takes in all of the results of all of the June and January regents exams for a given year
# It also takes in the section table from the June folder
# There should also be some way of indicating which courses are associated with which exams, and whether the course prepared the student for june or what.  Base this off of the info in MathRegents.R

# Calculate pass rates by 
#   exam, 
#   exam category
#   teacher
#   teacher-X-exam
#   teacher-X-category
# And do it for January, June, and Full Year.

# CopyClipboard()

JuneFolder = "\\\\stuthin2\\Data\\regents score exports\\2019-06"
JanuaryFolder = "\\\\stuthin2\\Data\\regents score exports\\2019-01"

Course2RegCat = read.xlsx(xlsxFile = PSLocation, sheet = "Course2RegCat")

sectionTable = read.xlsx(xlsxFile = "\\\\stuthin2\\Data\\regents score exports\\2019-06\\section table.xlsx", sheet = "Enrollments")
sectionTable$ExamCat = Course2RegCat$ExamCat[match(sectionTable$Course, Course2RegCat$Course)]
possibleFolders = data.frame(folder = testLookup$Code, stringsAsFactors = F)
possibleFolders = rbind.data.frame(possibleFolders, possibleFolders, stringsAsFactors = F)
possibleFolders$Session = "June"
possibleFolders$Session[1:(nrow(possibleFolders)/2)] = "January"
possibleFolders$ParentFolder = JuneFolder
possibleFolders$ParentFolder[1:(nrow(possibleFolders)/2)] = JanuaryFolder
possibleFolders$path = paste0(possibleFolders$ParentFolder,"/",possibleFolders$folder)
possibleFolders$exists = file.exists(possibleFolders$path)
possibleFolders = possibleFolders[possibleFolders$exists,]
possibleFolders$Exam = testLookup$Database[match(possibleFolders$folder, testLookup$Code)]
possibleFolders$ExamCat = testLookup$Category[match(possibleFolders$folder, testLookup$Code)]

allScores = vector(mode = "list", length = nrow(possibleFolders))
possibleFolders$ExamSession = paste0(possibleFolders$folder, "-", possibleFolders$Session)

i = 1
for(i in 1:length(allScores)){
  currentScores = read.csv(paste0(possibleFolders$path[i],"/upload_percentages.csv"), stringsAsFactors = F)
  currentScores$Session = possibleFolders$Session[i]
  currentScores$Exam = possibleFolders$Exam[i]
  currentScores$ExamCat = possibleFolders$ExamCat[i]
  allScores[[i]] = currentScores
}
names(allScores) = possibleFolders$ExamSession

str(allScores, max.level = 1)



# So, start by merging scores from the same session in the same category, keeping all best scores.
JuneGlobalTrans = allScores$`GlobTrans-June`
JuneGlobal2 = allScores$`Glob2-June`
JuneGlobalBoth = rbind.data.frame(JuneGlobal2, JuneGlobalTrans)
JuneGlobalBest = JuneGlobalBoth[!duplicated(JuneGlobalBoth$StudentID),]
JuneGlobalBest$Exam = "Best Global Score"
for(i in 1:nrow(JuneGlobalBest)){
  thisStudentScores = JuneGlobalBoth$Percentage[JuneGlobalBoth$StudentID == JuneGlobalBest$StudentID[i]]
  JuneGlobalBest$Percentage[i] = betterMax(thisStudentScores)
}


allScores2 = allScores
allScores2$`GlobTrans-June` = NULL
allScores2$`Glob2-June` = NULL
allScores2$`Glob2-June` = JuneGlobalBest

str(allScores2, max.level = 1)


# Then create a merged data set for each exam category with student info, june score, and january score
scoremerger = vector(mode = "list", length = 10)
names(scoremerger) = unique(possibleFolders$ExamCat)
for(i in 1:length(scoremerger)){
  x = vector(mode = "list", length = 3)
  names(x) = c("January", "June", "Best")
  scoremerger[[i]] = x
}

for(i in 1:length(allScores2)){
  ex.ses = names(allScores2[i])
  ex.ses = strsplit(ex.ses, "-")[[1]]
  thisCategory = testLookup$Category[testLookup$Code == ex.ses[1]]
  scoremerger[[thisCategory]][[ex.ses[2]]] = allScores2[[i]]
}

str(scoremerger, max.level = 2)
i = 10
for(i in 1:length(scoremerger)){
  June = scoremerger[[i]][["June"]]
  January = scoremerger[[i]][["January"]]
  Both = rbind.data.frame(June, January, stringsAsFactors = F)
  Best = Both[!duplicated(Both$StudentID),c("StudentID", "StudentName")]
  Best$JanScore = January$Percentage[match(Best$StudentID, January$StudentID)]
  Best$JuneScore = June$Percentage[match(Best$StudentID, June$StudentID)]
  if(!("JanScore" %in% colnames(Best))){
    Best$JanScore = NA
  }
  if(!("JuneScore" %in% colnames(Best))){
    Best$JuneScore = NA
  }
  scoremerger[[i]][["Best"]] = Best
}



# Then add columns a thru d: 
# a = In June Course
# b = In Jan Course
# c = Has June Score
# d = Has Jan Score
i = 10
j = 1
Course2RegCat = DFna.to.empty(Course2RegCat)
for(i in 1:length(scoremerger)){
  thisCategory = names(scoremerger)[i]
  relevantJuneCourses = Course2RegCat$Course[Course2RegCat$June == "Yes" & Course2RegCat$ExamCat == thisCategory]
  relevantJanuaryCourses = Course2RegCat$Course[Course2RegCat$January == "Yes" & Course2RegCat$ExamCat == thisCategory]
  Best = scoremerger[[i]][["Best"]]
  Best$HasJuneScore = !is.na(Best$JuneScore)
  Best$HasJanuaryScore = !is.na(Best$JanScore)
  Best$InJuneCourse = F
  Best$InJanuaryCourse = F
  for(j in 1:nrow(Best)){
    thisStudent = Best$StudentID[j]
    thisStudentCourses = sectionTable$Course[sectionTable$StudentID == thisStudent & sectionTable$ExamCat == thisCategory]
    Best$InJuneCourse[j] = length(intersect(thisStudentCourses, relevantJuneCourses)) > 0
    Best$InJanuaryCourse[j] = length(intersect(thisStudentCourses, relevantJanuaryCourses)) > 0
  } 
  scoremerger[[i]][["Best"]] = Best
}

# Apply the if statement to determine whether that student's score counts, and remove those that don't
# if((a & c) | (b & (c | d)))
# Add a column for Best Score

for(i in 1:length(scoremerger)){
  Best = scoremerger[[i]][["Best"]]
  Best$BestScore = VbetterMax(Best$JuneScore, Best$JanScore)
  Best$ScoreCounts = (Best$InJuneCourse & Best$HasJuneScore) | (Best$InJanuaryCourse & (Best$HasJuneScore | Best$HasJanuaryScore))
  scoremerger[[i]][["Best"]] = Best
}



str(sectionTable)
yearlongTeacherExamCat = sectionTable[!duplicated(sectionTable$TeacherExamCat), c("Teacher", "ExamCat", "TeacherExamCat")]
yearlongTeacherExamCat$Under65 = NA_integer_
yearlongTeacherExamCat$Passed = NA_integer_
yearlongTeacherExamCat$AtLeast80 = NA_integer_


# Loop through each row of yearlongTeacherExamCat
# For each row, identify the dataset associated with that exam category
# Limit the dataset to just those students whom the teacher taught in a relevant course
# Count the number of scores in the remaining data set that fall into each of the three categories and load those counts into yearlongTeacherExamCat
# Create a reduced data set with 1 row per teacher and keep the sums of that teacher's relevant rows
# Calculate pass rate

i = 1
for(i in 1:nrow(yearlongTeacherExamCat)){
  thisCat = yearlongTeacherExamCat$ExamCat[i]
  thisTeacher = yearlongTeacherExamCat$Teacher[i]
  thisTeacherExamCat = yearlongTeacherExamCat$TeacherExamCat[i]
  thisDataSet = scoremerger[[thisCat]][["Best"]]
  thisDataSet$hasThisTeacher = F
  relevantStudents = sectionTable$StudentID[sectionTable$TeacherExamCat == thisTeacherExamCat]
  relevantScores = thisDataSet$BestScore[thisDataSet$StudentID %in% relevantStudents]
  yearlongTeacherExamCat$Under65[i] = sum(relevantScores < 65)
  yearlongTeacherExamCat$AtLeast80[i] = sum(relevantScores >= 80)
  yearlongTeacherExamCat$Passed[i] = sum(relevantScores >= 65 & relevantScores < 80)
}


write.csv(yearlongTeacherExamCat, file = paste0(OutFolder, "teacher regents pass counts.csv"))

# Now the sped stuff
RoseStudents = c(171810585,151610258,161710473,171810656,171810753,161710537,161710539)
JohnsonStudents = c(171810586,151610381,151612026,171810617,151610364,161710446,181911274,181911254)
RollinStudents = c(171810626,181910997,181911023,171810867,181911279)

RoseScores = integer(0)
JohnsonScores = integer(0)
RollinScores = integer(0)
i = 1
for(i in 1:length(scoremerger)){
  Best = scoremerger[[i]][["Best"]]
  RoseScores = c(RoseScores, Best$BestScore[Best$StudentID %in% RoseStudents])
  JohnsonScores = c(JohnsonScores, Best$BestScore[Best$StudentID %in% JohnsonStudents])
  RollinScores = c(RollinScores, Best$BestScore[Best$StudentID %in% RollinStudents])
}


max(RoseScores)
max(JohnsonScores)
max(RollinScores)
mean(RoseScores > 65) * 100
mean(RoseScores > 80) * 100

mean(JohnsonScores > 65) * 100
mean(JohnsonScores > 80) * 100

mean(RollinScores > 65) * 100
mean(RollinScores > 80) * 100











