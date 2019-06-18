# PSATsAndMathCourses.R

# This provides the SAT Prep teacher with their students' PSAT scores and current math courses

CC.sat = cc.raw[cc.raw$`[02]course_name` == "SAT Prep/College Prep",]

CC.sat = CC.sat[CC.sat$TermID > 0,]

PSAT.2018 = PSAT.raw[PSAT.raw$Year == 2018,]

CC.sat$PSAT.Reading = PSAT.2018$Read[match(CC.sat$`[01]Student_Number`, PSAT.2018$ID)]
CC.sat$PSAT.Math = PSAT.2018$Math[match(CC.sat$`[01]Student_Number`, PSAT.2018$ID)]

MathCourses = FullAlignment$Course[FullAlignment$Subject == "Math"]
MathCourses = MathCourses[!is.na(MathCourses)]

MathEnrollments = cc.raw[cc.raw$`[01]Student_Number` %in% CC.sat$`[01]Student_Number`,]
MathEnrollments = MathEnrollments[MathEnrollments$`[02]course_name` %in% MathCourses,]
MathEnrollments = MathEnrollments[MathEnrollments$TermID > 0,]

CC.sat$Math.Course = "None"

i = 1
for(i in 1:nrow(CC.sat)){
  thisStudent = CC.sat$`[01]Student_Number`[i]
  thisCourseSet = MathEnrollments$`[02]course_name`[MathEnrollments$`[01]Student_Number` == thisStudent]
  if(length(thisCourseSet) > 1){
    CC.sat$Math.Course[i] = paste0(thisCourseSet, collapse = " & ")
  } else if(length(thisCourseSet) == 1){
    CC.sat$Math.Course[i] = thisCourseSet
  }
}

unique(CC.sat$Math.Course)


write.csv(x = CC.sat, file = paste0(OutFolder, "SAT student info.csv"))
