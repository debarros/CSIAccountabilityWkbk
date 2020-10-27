# MathCourseAssignments.R

# This script takes all students and determines what math course they should take the next year.

# fill the asapExports.xlsx file by exporting the Student List By Building files from ASAP.
# Fill in graduates.csv with the students who just graduated in June

# This needs to start taking into account students who are active but unenrolled, and not assign them a math class

#-------------------------------#
#### Copy, set, or read data ####
#-------------------------------#

currentTerm = 2800                    # Use the term ID of the year that just ended
powerschool = powerschoolraw          # Make a copy of the powerschool data
curStuds = currentStudents             # Make a copy of the table of current students
graduates = read.csv("graduates.csv") # read in the graduates
newAlg1 = read.xlsx(xlsxFile = "asapExports.xlsx", sheet = "Alg1")
newGeom = read.xlsx(xlsxFile = "asapExports.xlsx", sheet = "Geom")
newAlg2 = read.xlsx(xlsxFile = "asapExports.xlsx", sheet = "Alg2")
F2.temp = F2

# filter the grade file
mathgrades = F2.temp[F2.temp$TermID == currentTerm,]
mathgrades$subject = FullAlignment$Subject[match(mathgrades$Course_Name, FullAlignment$Course)]
mathgrades[(is.na(mathgrades$subject)),]
mathgrades = mathgrades[!(is.na(mathgrades$subject)),]

mathgrades = mathgrades[mathgrades$subject == "Math",]
mathgrades = mathgrades[mathgrades$Course_Name != "Exploring Algebra with Technology",]
mathgrades = mathgrades[mathgrades$SchoolName == "Green Tech High Charter School",]

# whowasnew = mathgrades[mathgrades$`[1]Student_Number` > 181900000,]
# summary(factor(whowasnew$Course_Name))

# The next lines remove graduates and transfers
mathgrades = mathgrades[!(mathgrades$`[1]Student_Number` %in% graduates$ID),]                # remove grads
mathgrades = mathgrades[mathgrades$`[1]Student_Number` %in% curStuds$Student_Number,] # remove anyone who is not a current student
curStuds = curStuds[!(curStuds$Student_Number %in% graduates$ID),]

unique(mathgrades$Course_Name)

# Determine the number of math courses, which ones, and how many F's the student earned
curStuds$currentCourse = NA
curStuds$CourseCount = 0
curStuds$Fcount = 0
for(i in 1:nrow(curStuds)){
  curMathGrades = mathgrades[mathgrades$`[1]Student_Number` == curStuds$Student_Number[i],]
  if(nrow(curMathGrades) > 0){
    curCourses = curMathGrades$Course_Name
    curStuds$CourseCount[i] = nrow(curMathGrades)
    curStuds$Fcount[i] = sum(curMathGrades$Grade == "F")
    curStuds$currentCourse[i] = paste0(sort(curCourses), collapse = "-")
  }
}

unique(curStuds$currentCourse)

# Determine each student's best score on each exam
curStuds$oldAlg1 = powerschool$Regents_Algebra_Score[match(curStuds$Student_Number, powerschool$student_number)]
curStuds$oldAlg1[is.na(curStuds$oldAlg1)] = 0
curStuds$oldGeom = powerschool$Regents_Geometry_Score[match(curStuds$Student_Number, powerschool$student_number)]
curStuds$oldGeom[is.na(curStuds$oldGeom)] = 0
curStuds$oldAlg2 = powerschool$`Regents_Algebra2/Trigonometry_Score`[match(curStuds$Student_Number, powerschool$student_number)]
curStuds$oldAlg2[is.na(curStuds$oldAlg2)] = 0

curStuds$newAlg1 = newAlg1$ScaledScore_1[match(curStuds$Student_Number, newAlg1$StudentID_1)]
curStuds$newGeom = newGeom$ScaledScore_1[match(curStuds$Student_Number, newGeom$StudentID_1)]
curStuds$newAlg2 = newAlg2$ScaledScore_1[match(curStuds$Student_Number, newAlg2$StudentID_1)]

curStuds$Alg1 = VbetterMax(curStuds$oldAlg1, curStuds$newAlg1)
curStuds$Geom = VbetterMax(curStuds$oldGeom, curStuds$newGeom)
curStuds$Alg2 = VbetterMax(curStuds$oldAlg2, curStuds$newAlg2)




#---------------------------#
#### Perform the sorting ####
#---------------------------#

# This will need to updated based on changes to the math course assignment filter
# Also, it would be better if this could use some sort of coded instructions tab

curStuds$nextCourse = NA
for(i in 1:nrow(curStuds)){
  
  if(is.na(curStuds$currentCourse[i])){
    curStuds$nextCourse[i] = "none"
    
  } else if(curStuds$currentCourse[i] == "Algebra A"){
    curStuds$nextCourse[i] = "Alg1 R"
    if(curStuds$Fcount[i] > 0){
      curStuds$nextCourse[i] = "Alg A"
    }
    
  } else if(curStuds$currentCourse[i] == "Algebra II/Trig- 1 credit"){
    if(curStuds$Fcount[i] > 0){
      curStuds$nextCourse[i] = "Alg2"
    } else {
      curStuds$nextCourse[i] = "Intermediate Trig"
      if(curStuds$Alg2[i] >= 65){
        curStuds$nextCourse[i] = "Precalc"
      }
    }
    
  } else if(curStuds$currentCourse[i] == "Geometry 1 credit"){
    curStuds$nextCourse[i] = "Geom R"
    if(curStuds$Fcount[i] == 0){
      curStuds$nextCourse[i] = "Alg2"
      if(curStuds$Grade_Level[i] == 11){
        curStuds$nextCourse[i] = "Alg2"
      }
    }
    
  } else if(curStuds$currentCourse[i] == "Geometry A"){
    curStuds$nextCourse[i] = "Geom R"
    if(curStuds$Fcount[i] > 0 & curStuds$Alg1[i] < 75){
      curStuds$nextCourse[i] = "Geom A"
    }
    
  } else if(curStuds$currentCourse[i] == "Integrated Algebra I- 1 credit"){
    curStuds$nextCourse[i] = "Alg1 R"
    if(curStuds$Fcount[i] == 0){
      curStuds$nextCourse[i] = "Interm Alg"
      if(curStuds$Alg1[i] >= 65) curStuds$nextCourse[i] = "Geom A"
      if(curStuds$Alg1[i] >= 75) curStuds$nextCourse[i] = "Geom R"
    }
    
  } else if(curStuds$currentCourse[i] == "Intermediate Algebra 10"){
    curStuds$nextCourse[i] = "Geom R"
    if(curStuds$Alg1[i] < 75) curStuds$nextCourse[i] = "Geom A"
    if(curStuds$Alg1[i] < 65 & curStuds$Fcount[i] > 0) curStuds$nextCourse[i] = "Interm Alg"
    
  } else if(curStuds$currentCourse[i] == "Intermediate Trig"){
    curStuds$nextCourse[i] = "Precalc"
    if(curStuds$Alg2[i] < 65 & curStuds$Fcount[i] > 0) {
      curStuds$nextCourse[i] = "Intermediate Trig"
    }
    
  } else if(curStuds$currentCourse[i] == "Topics in Math/ Finite Math"){
    curStuds$nextCourse[i] = "Precalc"
    if(curStuds$Fcount[i] > 0 & curStuds$Alg2[i] < 65) {
      curStuds$nextCourse[i] = "Intermediate Trig"
    }
    
  } else if(curStuds$currentCourse[i] == "Pre-Calculus 1-credit"){
    curStuds$nextCourse[i] = "Calculus"
    if(curStuds$Fcount[i] > 0) curStuds$nextCourse[i] = "Precalc"
  }
}

summary(as.factor(curStuds$nextCourse))
unique(as.factor(curStuds$nextCourse))

table(curStuds$currentCourse, curStuds$nextCourse)
summary(as.factor(curStuds$currentCourse))


curStuds[is.na(curStuds$currentCourse),]

curStuds$Student_Number[is.na(curStuds$currentCourse)]
curStuds = curStuds[!is.na(curStuds$currentCourse),]
curStuds[is.na(curStuds$nextCourse),]

# Those have NA next course are students who had multiple courses in the year that just ended

# Generate output
write.csv(x = curStuds, file = paste0(OutFolder,"math course assignments.csv"))



