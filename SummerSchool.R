# SummerSchool.R


regentsScores = read.xlsx(xlsxFile = PSLocation, sheet = "Regents long")
grades = currentGrades
courses = read.xlsx(xlsxFile = PSLocation, sheet = "Summer Courses")


grades$passingScore = courses$Passing[match(grades$Course, courses$School.Year.Course)]
grades$SummerCourse = courses$Equivalent.Summer.Course[match(grades$Course, courses$School.Year.Course)]
grades$PointsOwed = grades$Pct - grades$passingScore
grades$EligibleForSummer = "Passed"
grades$EligibleForSummer[grades$PointsOwed < 0] = "Yes"
grades$EligibleForSummer[grades$PointsOwed < -10] = "No"
grades$SummerCourseOffered = courses$`Summer.Course.Offered?`[match(grades$Course, courses$School.Year.Course)]
grades$OnPlato = courses$`Equivalent.Course.on.PLATO?`[match(grades$Course, courses$School.Year.Course)]
grades$Critical = courses$`Critical?.(school.year.course.terminates.in.core.regents.exam)`[match(grades$Course, courses$School.Year.Course)]
grades$AssociatedRegents = courses$Associated.Regents[match(grades$Course, courses$School.Year.Course)]

grades$RegentsScore = 0
for(i in 1:nrow(grades)){
  currentID = grades$Student.Number[i]
  currentExam = grades$AssociatedRegents[i]
  currentScoreSet = regentsScores$Score[regentsScores$CountsFor == currentExam & regentsScores$StudentNumber == currentID]
  if(length(currentScoreSet) > 0){
    grades$RegentsScore[i] = betterMax(currentScoreSet)
  }
}

grades$PassedRegents = grades$RegentsScore >= 65

grades$ThePlan = "passed"
for(i in 1:nrow(grades)){
  if(grades$EligibleForSummer[i] == "Passed"){
    
  } else if(grades$EligibleForSummer[i] == "Yes"){
    if(grades$SummerCourseOffered[i] != "No"){
      grades$ThePlan[i] = "Possibly Summer"
    } else {                                        # if the course is not being offered this summer
      if(grades$OnPlato[i] == "Yes"){
        grades$ThePlan[i] = "Summer on Plato"  
      } else {                                      # if it's not on plato
        grades$ThePlan[i] = "Retake next year"  
      }
    }
  } else {                                          # if it's not eligible for summer school
    if(grades$OnPlato[i] == "Yes"){
      if(grades$Critical[i] == "Yes"){
        if(grades$PassedRegents[i]){
          grades$ThePlan[i] = "Retake on Plato"  
        } else {                                    # if the student didn't pass the regents
          grades$ThePlan[i] = "Retake next year"  
        }
      } else {                                      # if it's not critical
        grades$ThePlan[i] = "Retake on Plato"  
      }
    } else {                                        # if it's not on Plato
      grades$ThePlan[i] = "Retake next year"  
    }
  }
}



students = grades[!duplicated(grades$Student.Number),1:2]

students$SummerNeeds = 0
students$CriticalSummerNeeds = 0
students$NonCriticalSummerNeedsAvailableOnPlato = 0
for(i in 1:nrow(students)){
  currentID = students$Student.Number[i]
  students$SummerNeeds[i] = sum(grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer")
  students$CriticalSummerNeeds[i] = sum(grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer" & grades$Critical == "Yes")
  students$NonCriticalSummerNeedsAvailableOnPlato[i] = sum(
    grades$Student.Number == currentID & 
      grades$ThePlan == "Possibly Summer" & 
      grades$Critical == "No" & 
      grades$OnPlato == "Yes")
}



for(i in 1:nrow(students)){
  print(i)
  currentID = students$Student.Number[i]
  if(students$SummerNeeds[i] %in% 1:2){
    grades$ThePlan[grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer"] = "Definitely Summer"
  } else if(students$SummerNeeds[i] > 2){
    if(students$CriticalSummerNeeds[i] == 1){
      grades$ThePlan[grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer" & grades$Critical == "Yes"] = "Definitely Summer"
    } else if(students$CriticalSummerNeeds[i] == 2){
      grades$ThePlan[grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer" & grades$Critical == "Yes"] = "Definitely Summer"
      grades$ThePlan[grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer" & grades$Critical == "No" & grades$OnPlato == "Yes"] = "Summer on Plato"
      grades$ThePlan[grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer" & grades$Critical == "No" & grades$OnPlato == "No"] = "Retake next year"
    } else if(students$CriticalSummerNeeds[i] == 0){
      if(students$SummerNeeds[i] - students$NonCriticalSummerNeedsAvailableOnPlato[i] <= 2){
        grades$ThePlan[grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer" & grades$Critical == "No" & grades$OnPlato == "No"] = "Definitely Summer"
        grades$ThePlan[grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer" & grades$Critical == "No" & grades$OnPlato == "Yes"] = "Summer on Plato"
      } 
    }
  }
}

summary(as.factor(grades$ThePlan))


students$IssuesRemain = 0
for(i in 1:nrow(students)){
  currentID = students$Student.Number[i]
  students$IssuesRemain[i] = sum(grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer")
}

students[students$IssuesRemain > 0,]

grades[grades$ThePlan == "Possibly Summer",]

SummerCourses = courses[!duplicated(courses$Equivalent.Summer.Course),
                        c("Equivalent.Summer.Course", "Summer.Course.Offered?", "Subject")]
rownames(SummerCourses) = NULL

SummerCourses = SummerCourses[SummerCourses$`Summer.Course.Offered?` != "No",]
rownames(SummerCourses) = NULL

SummerCourses$StudentCount = 0
for(i in 2:nrow(SummerCourses)){
  curSumCrse = SummerCourses$Equivalent.Summer.Course[i]
  SummerCourses$StudentCount[i] = sum(grades$ThePlan == "Definitely Summer" & grades$SummerCourse == curSumCrse)
}


i = 9
SummerCourses$SectionCount = ceiling(SummerCourses$StudentCount/12)
SummerCourses$SectionCount[SummerCourses$StudentCount < 4] = 0

SumCrse.single = SummerCourses[SummerCourses$SectionCount == 1,]


Pairings = as.data.frame(t(combn(x = SumCrse.single$Equivalent.Summer.Course, m = 2)), stringsAsFactors = F)

Pairings$StudentCount = 0

for(i in 1:nrow(Pairings)){
  firstCourseIDs = grades$Student.Number[grades$SummerCourse == Pairings$V1[i] & grades$ThePlan == "Definitely Summer"]
  print(paste0(Pairings$V1[i], " - ", length(firstCourseIDs)))
  secondCourseIDs = grades$Student.Number[grades$SummerCourse == Pairings$V2[i] & grades$ThePlan == "Definitely Summer"]
  print(paste0(Pairings$V2[i], " - ", length(firstCourseIDs)))
  Combo = intersect(firstCourseIDs, secondCourseIDs)
  Pairings$StudentCount[i] = length(Combo)
}



Pairings.concern = Pairings[Pairings$StudentCount > 0,]
Pairings.concern$Keep = T


keepgoing = T
while(keepgoing){
  rowcount = nrow(Pairings.concern)
  pairingConcernCourses = unique(c(Pairings.concern$V1, Pairings.concern$V2))
  
  for(i in pairingConcernCourses){
    occurances = sum(Pairings.concern$V1 == i) + sum(Pairings.concern$V2 == i)
    if(occurances == 1){
      Pairings.concern$Keep[Pairings.concern$V1 == i | Pairings.concern$V2 == i] = F
    }
  }
  
  Pairings.concern = Pairings.concern[Pairings.concern$Keep,]
  
  newrowcount = nrow(Pairings.concern)
  if(newrowcount == rowcount){
    keepgoing = F
  }
}

print(Pairings.concern)

