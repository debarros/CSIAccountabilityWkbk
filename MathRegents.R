# MathRegents.R

# This script determines which students should be signed up for each math regents exam
# It requires the current year cc table, all F2 grades, the table of current students, and the regents database

Students = currentStudents # make a copy of the table of current students
cc.cur = cc.raw[cc.raw$TermID > 0,] # make a copy of the cc table, limiting to current enrollments
cc.cur$dept = FullAlignment$Subject[match(cc.cur$`[02]course_name`, FullAlignment$Course)] # add course-subject alignments
cc.cur.math = cc.cur[cc.cur$dept == "Math",] # limit to just math
cc.cur.math = cc.cur.math[cc.cur.math$`[02]course_name` != "Exploring Algebra with Technology",] # remove EAT
row.names(cc.cur.math) = NULL


# Get best regents scores by category
cols2use = grepl(pattern = "Score", x = colnames(RegentsDBraw), ignore.case = T)
cols2use[1] = T
Regents = RegentsDBraw[,cols2use]
Regents = Regents[Regents$StudentNumber %in% Students$Student_Number,]
Regents$Alg1 = apply(Regents[,c("AlgScore", "AlgCCScore")], 1, betterMax)
Regents$Geom = apply(Regents[,c("GeomScore", "GeomCCScore")], 1, betterMax)
Regents$Alg2 = apply(Regents[,c("TrigScore", "Alg2CCScore")], 1, betterMax)


# Limit the stored F2 grades to just math grades
F2$dept = FullAlignment$Subject[match(F2$Course_Name, FullAlignment$Course)] # add course-subject alignments
F2 = F2[!is.na(F2$dept),]
F2 = F2[F2$dept == "Math",]
F2 = F2[F2$Course_Name != "Exploring Algebra with Technology",]
F2 = F2[!is.na(F2$Grade),]


# This is not a great way to make this data.frame
MathCourses = data.frame(
  Course = c("Algebra A", "Integrated Algebra I- 1 credit", "Intermediate Algebra 10", "Geometry A", "Geometry 1 credit", 
             "Algebra II/Trig- 1 credit", "Intermediate Trig", "Topics in Math/ Finite Math"),
  Order = 1:8,
  JuneAlg1 = c(F, rep(T, 7)),
  JanAlg1 = c(F,F, rep(T, 6)),
  RelAlg1 = c(F, rep(T, 2), rep(F, 5)),
  JuneGeom = c(rep(F, 4), rep(T, 4)),
  JanGeom = c(rep(F, 4), rep(T, 4)),
  RelGeom = c(rep(F, 4), T, rep(F, 3)),
  JuneAlg2 = c(rep(F, 5), rep(T, 3)),
  JanAlg2 = c(rep(F, 6), rep(T, 2)),
  RelAlg2 = c(rep(F, 5), rep(T, 2), F),
  stringsAsFactors = F
)

# This should be character(0).  If not, additional rows must be added to the MathCourses data.frame
setdiff(unique(cc.cur.math$`[02]course_name`), MathCourses$Course)


# Add the necessary variables to the Students table
Students$MathCourseCount = 0
Students$HighestCurrentMathCourse = ""
Students$HighPassedMathCourse = ""
Students$EligibleAlg1 = FALSE
Students$EligibleGeom = FALSE
Students$EligibleAlg2 = FALSE
Students$ShouldTakeAlg1 = FALSE
Students$ShouldTakeGeom = FALSE
Students$ShouldTakeAlg2 = FALSE



# Fill in the relevant info
Students$BestAlg1 = Regents$Alg1[match(Students$Student_Number, Regents$StudentNumber)]
Students$BestGeom = Regents$Geom[match(Students$Student_Number, Regents$StudentNumber)]
Students$BestAlg2 = Regents$Alg2[match(Students$Student_Number, Regents$StudentNumber)]

Students$HighestExamTaken[!is.na(Students$BestAlg1)] = "Alg1"
Students$HighestExamTaken[!is.na(Students$BestGeom)] = "Geom"
Students$HighestExamTaken[!is.na(Students$BestAlg2)] = "Alg2"

for(i in 1:nrow(Students)){
  thisCourses = cc.cur.math$`[02]course_name`[cc.cur.math$`[01]Student_Number` == Students$Student_Number[i]]
  thisCourses = MathCourses[MathCourses$Course %in% thisCourses,]
  
  passedCourses = F2[F2$Grade != "F",]
  passedCourses = passedCourses$Course_Name[passedCourses$`[1]Student_Number` == Students$Student_Number[i]]
  passedCourses = MathCourses[MathCourses$Course %in% passedCourses,]
  
  Students$MathCourseCount[i] = nrow(thisCourses)
  if(nrow(thisCourses) > 0){
    Students$HighestCurrentMathCourse[i] = thisCourses$Course[thisCourses$Order == max(thisCourses$Order)]
  }
  
  if(nrow(passedCourses) > 0){
    Students$HighPassedMathCourse[i] = passedCourses$Course[passedCourses$Order == max(passedCourses$Order)]  
  }
  
  Students$EligibleAlg1[i] = any(c(passedCourses$JuneAlg1, thisCourses$JuneAlg1))
  Students$EligibleGeom[i] = any(c(passedCourses$JuneGeom, thisCourses$JuneGeom))
  Students$EligibleAlg2[i] = any(c(passedCourses$JuneAlg2, thisCourses$JuneAlg2))
  
  
  if(any(c(Students$EligibleAlg1[i], !is.na(Students$BestAlg1[i])))){
    if(any(c(is.na(Students$BestAlg1[i]), thisCourses$RelAlg1))){
      Students$ShouldTakeAlg1[i] = T
    } else if(Students$BestAlg1[i] < 65){
      Students$ShouldTakeAlg1[i] = T
    }
  }
  
  if(any(c(Students$EligibleGeom[i], !is.na(Students$BestGeom[i])))){
    if(any(c(is.na(Students$BestGeom[i]), thisCourses$RelGeom))){
      Students$ShouldTakeGeom[i] = T
    } else if(Students$BestGeom[i] < 65){
      Students$ShouldTakeGeom[i] = T
    }
  }
  
  if(any(c(Students$EligibleAlg2[i], !is.na(Students$BestAlg2[i])))){
    if(any(c(is.na(Students$BestAlg2[i]), thisCourses$RelAlg2))){
      Students$ShouldTakeAlg2[i] = T
    } else if(Students$BestAlg2[i] < 65){
      Students$ShouldTakeAlg2[i] = T
    }
  }
}



write.csv(Students, paste0(OutFolder, "math regents.csv"))







#---------------------------------------------------------------------------------#
#### Find situations where a student is taking a course that he already passed ####
#---------------------------------------------------------------------------------#

# Just math
F2$code = paste0(F2$`[1]Student_Number`, " - ", F2$Course_Name)
F2.passed = F2[F2$Grade != "F",]
cc.cur.math$code = paste0(cc.cur.math$`[01]Student_Number`, " - ", cc.cur.math$`[02]course_name`)
intersect(F2.passed$code, cc.cur.math$code)
View(F2.passed[F2.passed$code %in% cc.cur.math$code,])

# All courses
F2$code = paste0(F2$`[1]Student_Number`, " - ", F2$Course_Name)
F2.passed = F2[!is.na(F2$Grade),]
F2.passed = F2.passed[F2.passed$Grade != "F",]
F2.passed = F2.passed[F2.passed$TermID < 2700,]
cc.cur$code = paste0(cc.cur$`[01]Student_Number`, " - ", cc.cur$`[02]course_name`)
intersect(F2.passed$code, cc.cur$code)
View(F2.passed[F2.passed$code %in% cc.cur$code,])

unique(F2.passed$Course_Name[F2.passed$code %in% cc.cur$code])


write.csv(F2.passed[F2.passed$code %in% cc.cur$code,], paste0(OutFolder, "retaking passed courses.csv"))