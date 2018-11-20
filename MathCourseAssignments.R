# MathCourseAssignments.R

# fill the asapExports.xlsx file by exporting the Student List By Building files from ASAP.


# Copy, set, or read data
currentTerm = 2700                    # Use the term ID of the year that just ended
powerschool = powerschoolraw          # Make a copy of the powerschool data
graduates = read.csv("graduates.csv") # read in the graduates
newAlg1 = read.xlsx(xlsxFile = "asapExports.xlsx", sheet = "Alg1")
newGeom = read.xlsx(xlsxFile = "asapExports.xlsx", sheet = "Geom")
newAlg2 = read.xlsx(xlsxFile = "asapExports.xlsx", sheet = "Alg2")


# filter the grade file
mathgrades = F2[F2$TermID == currentTerm,]
mathgrades$subject = FullAlignment$Subject[match(mathgrades$Course_Name, FullAlignment$Course)]
mathgrades[(is.na(mathgrades$subject)),]
mathgrades = mathgrades[!(is.na(mathgrades$subject)),]

mathgrades = mathgrades[mathgrades$subject == "Math",]
mathgrades = mathgrades[mathgrades$Course_Name != "Exploring Algebra with Technology",]
mathgrades = mathgrades[mathgrades$SchoolName == "Green Tech High Charter School",]

# whowasnew = mathgrades[mathgrades$`[1]Student_Number` > 161700000,]
# summary(factor(whowasnew$Course_Name))

# The next lines remove graduates and transfers
mathgrades = mathgrades[!(mathgrades$`[1]Student_Number` %in% graduates$ID),]                # remove grads
mathgrades = mathgrades[mathgrades$`[1]Student_Number` %in% currentStudents$Student_Number,] # remove anyone who is not a current student

unique(mathgrades$Course_Name)

# Determine the number of math courses, which ones, and how many F's the student earned
currentStudents$currentCourse = NA
currentStudents$CourseCount = 0
currentStudents$Fcount = 0
for(i in 1:nrow(currentStudents)){
  curMathGrades = mathgrades[mathgrades$`[1]Student_Number` == currentStudents$Student_Number[i],]
  if(nrow(curMathGrades) > 0){
    curCourses = curMathGrades$Course_Name
    currentStudents$CourseCount[i] = nrow(curMathGrades)
    currentStudents$Fcount[i] = sum(curMathGrades$Grade == "F")
    currentStudents$currentCourse[i] = paste0(sort(curCourses), collapse = "-")
  }
}


# Determine each student's best score on each exam
currentStudents$oldAlg1 = powerschool$Regents_Algebra_Score[match(currentStudents$Student_Number, powerschool$student_number)]
currentStudents$oldAlg1[is.na(currentStudents$oldAlg1)] = 0
currentStudents$oldGeom = powerschool$Regents_Geometry_Score[match(currentStudents$Student_Number, powerschool$student_number)]
currentStudents$oldGeom[is.na(currentStudents$oldGeom)] = 0
currentStudents$oldAlg2 = powerschool$`Regents_Algebra2/Trigonometry_Score`[match(currentStudents$Student_Number, powerschool$student_number)]
currentStudents$oldAlg2[is.na(currentStudents$oldAlg2)] = 0

currentStudents$newAlg1 = newAlg1$ScaledScore_1[match(currentStudents$Student_Number, newAlg1$StudentID_1)]
currentStudents$newGeom = newGeom$ScaledScore_1[match(currentStudents$Student_Number, newGeom$StudentID_1)]
currentStudents$newAlg2 = newAlg2$ScaledScore_1[match(currentStudents$Student_Number, newAlg2$StudentID_1)]

currentStudents$Alg1 = VbetterMax(currentStudents$oldAlg1, currentStudents$newAlg1)
currentStudents$Geom = VbetterMax(currentStudents$oldGeom, currentStudents$newGeom)
currentStudents$Alg2 = VbetterMax(currentStudents$oldAlg2, currentStudents$newAlg2)

# Perform the sorting
# This will need to updated based on changes to the math course assignment filter
currentStudents$nextCourse = NA
for(i in 1:nrow(currentStudents)){
  if(is.na(currentStudents$currentCourse[i])){
    currentStudents$nextCourse[i] = "none"
  } else if(currentStudents$currentCourse[i] == "Algebra A"){
    currentStudents$nextCourse[i] = "Alg1 R"
    if(currentStudents$Fcount[i] > 0){
      currentStudents$nextCourse[i] = "Alg A"
    }
  } else if(currentStudents$currentCourse[i] == "Algebra II/Trig- 1 credit"){
    if(currentStudents$Fcount[i] > 0){
      currentStudents$nextCourse[i] = "Alg2"
    } else {
      currentStudents$nextCourse[i] = "Intermediate Trig"
      if(currentStudents$Alg2[i] >= 65){
        currentStudents$nextCourse[i] = "Precalc"
      }
    }
  } else if(currentStudents$currentCourse[i] == "Geometry 1 credit"){
    currentStudents$nextCourse[i] = "Geom R"
    if(currentStudents$Fcount[i] == 0){
      currentStudents$nextCourse[i] = "Alg2"
      if(currentStudents$Grade_Level[i] == 11){
        currentStudents$nextCourse[i] = "Alg2"
      }
    }
  } else if(currentStudents$currentCourse[i] == "Geometry A"){
    currentStudents$nextCourse[i] = "Geom R"
    if(currentStudents$Fcount[i] > 0 & currentStudents$Alg1[i] < 75){
      currentStudents$nextCourse[i] = "Geom A"
    }
  } else if(currentStudents$currentCourse[i] == "Integrated Algebra I- 1 credit"){
    currentStudents$nextCourse[i] = "Alg1 R"
    if(currentStudents$Fcount[i] == 0){
      currentStudents$nextCourse[i] = "Interm Alg"
      if(currentStudents$Alg1[i] >= 65) currentStudents$nextCourse[i] = "Geom A"
      if(currentStudents$Alg1[i] >= 75) currentStudents$nextCourse[i] = "Geom R"
    }
  } else if(currentStudents$currentCourse[i] == "Intermediate Algebra 10"){
    currentStudents$nextCourse[i] = "Geom R"
    if(currentStudents$Alg1[i] < 75) currentStudents$nextCourse[i] = "Geom A"
    if(currentStudents$Alg1[i] < 65 & currentStudents$Fcount[i] > 0) currentStudents$nextCourse[i] = "Interm Alg"
    
  } else if(currentStudents$currentCourse[i] == "Intermediate Trig"){
    currentStudents$nextCourse[i] = "Precalc"
    if(currentStudents$Alg2[i] < 65 & currentStudents$Fcount[i] > 0) {
      currentStudents$nextCourse[i] = "Intermediate Trig"
    } 
  } else if(currentStudents$currentCourse[i] == "Topics in Math/ Finite Math"){
    currentStudents$nextCourse[i] = "Precalc"
    if(currentStudents$Fcount[i] > 0 & currentStudents$Alg2[i] < 65) currentStudents$nextCourse[i] = "Intermediate Trig"
  }
}


summary(as.factor(currentStudents$nextCourse))
unique(as.factor(currentStudents$nextCourse))

table(currentStudents$currentCourse, currentStudents$nextCourse)


# Generate output
write.csv(x = currentStudents, file = paste0(OutFolder,"math course assignments.csv"))
