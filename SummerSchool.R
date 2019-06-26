# SummerSchool.R

# Load data
# This requires the functions.R file be sourced and the unstoredgrades be loaded
regentsScores = read.xlsx(xlsxFile = PSLocation, sheet = "Regents long")
grades = unstoredGrades
courses = read.xlsx(xlsxFile = PSLocation, sheet = "Summer Courses")

# Add additional information and select a plan for each row in the grades data.frame
grades = SummerSchool1(grades, courses, regentsScores)

# Set up a data.frame of just students
students = SummerSchool2(grades)

# Resolve the "Possibly Summer" situations that can be handled automatically.
# This is where the rule that students can only take 2 summer school courses is applied
grades = SummerSchool3(grades, students)

# Determine which students have issues yet to be resolved (i.e. "Possibly Summer" plans that could not be resolved automatically)
students$IssuesRemain = 0
for(i in 1:nrow(students)){
  currentID = students$Student.Number[i]
  students$IssuesRemain[i] = sum(grades$Student.Number == currentID & grades$ThePlan == "Possibly Summer")
}

# In the grades data.frame, indicate whether the student associated with each record still has issues to resolve
grades$StudentHasIssues = students$IssuesRemain[match(grades$Student.Number, students$Student.Number)]

# Get the subset of grades that are not passing and order it to allow for resolving issues
SummerGrades = grades[grades$ThePlan != "passed",]
SummerGrades = SummerGrades[order(SummerGrades$StudentHasIssues, 
                                  SummerGrades$Student.Number, 
                                  SummerGrades$ThePlan == "Possibly Summer", 
                                  decreasing = T),]

# Output the file for manual issue resolution
write.csv(x = SummerGrades, file = paste0(OutFolder, "SummerGrades table.csv"))

# At this point, go through the file and fix the situations in where students still have "Possibly Summer".

# Read in the updated table of grades that are not passing
RevisedGrades = read.csv(file = paste0(OutFolder, "SummerGrades table.csv"), stringsAsFactors = F)

# Get a set of unique summer courses and remove those not being offered
SummerCourses = SummerSchool4(courses, RevisedGrades)
write.csv(x = SummerCourses[SummerCourses$SectionCount > 0,], file = paste0(OutFolder,"table of summer course sections.csv"))

# Get the set of summer course singletons (only 1 section being offered)
SumCrse.single = SummerCourses[SummerCourses$SectionCount == 1,]


# Pairings of any courses and how many students are taking that pair
Pairings.all = as.data.frame(t(combn(x = SummerCourses$Equivalent.Summer.Course[SummerCourses$SectionCount > 0], m = 2)), stringsAsFactors = F)
Pairings.all$StudentCount = 0
for(i in 1:nrow(Pairings.all)){
  firstCourseIDs = grades$Student.Number[grades$SummerCourse == Pairings.all$V1[i] & grades$ThePlan == "Definitely Summer"]
  secondCourseIDs = grades$Student.Number[grades$SummerCourse == Pairings.all$V2[i] & grades$ThePlan == "Definitely Summer"]
  Combo = intersect(firstCourseIDs, secondCourseIDs)
  Pairings.all$StudentCount[i] = length(Combo)
}
Pairings.all = Pairings.all[Pairings.all$StudentCount >0,]



# Determine all possible pairings of singelton courses and count how many students are taking that pair
Pairings = as.data.frame(t(combn(x = SumCrse.single$Equivalent.Summer.Course, m = 2)), stringsAsFactors = F)
Pairings$StudentCount = 0
for(i in 1:nrow(Pairings)){
  firstCourseIDs = grades$Student.Number[grades$SummerCourse == Pairings$V1[i] & grades$ThePlan == "Definitely Summer"]
  secondCourseIDs = grades$Student.Number[grades$SummerCourse == Pairings$V2[i] & grades$ThePlan == "Definitely Summer"]
  Combo = intersect(firstCourseIDs, secondCourseIDs)
  Pairings$StudentCount[i] = length(Combo)
}

# Determine the pairings of singleton courses that students need
Pairings.separate = Pairings[Pairings$StudentCount > 0,]
print(Pairings.separate)

# Narrow the needed pairings of singleton courses down to unresolvable problems
Pairings.concern = Pairings.separate
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

ScheduleDemands = data.frame(course = unique(c(Pairings.separate$V1, Pairings.separate$V2)), stringsAsFactors = F)
ScheduleDemands$session = 0
ScheduleDemands$session[1] = 1

for(iter in 1:nrow(ScheduleDemands)){
  for(i in 1:nrow(ScheduleDemands)){
    curCourse = ScheduleDemands$course[i]
    curSession = ScheduleDemands$session[i]
    if(curSession > 0){
      conflictCourses = unique(c(
        Pairings.separate$V1[Pairings.separate$V2 == curCourse],
        Pairings.separate$V2[Pairings.separate$V1 == curCourse]))
      ScheduleDemands$session[ScheduleDemands$course %in% conflictCourses] = 3 - curSession
    }
  }
}

print(ScheduleDemands)
write.csv(x = ScheduleDemands, file = paste0(OutFolder, "schedule demands.csv"))

# For each student, determine the number of various plans for failed courses
students = SummerSchool5(students, RevisedGrades)

# Create a counter in the RevisedGrades data.frame that indicates which record it is for that particular student
RevisedGrades$StudentCount = 0
students$MarkedRedo = 0
for(i in 1:nrow(RevisedGrades)){
  curID = RevisedGrades$Student.Number[i]
  RevisedGrades$StudentCount[i] = students$MarkedRedo[students$Student.Number == curID] + 1
  students$MarkedRedo[students$Student.Number == curID] = RevisedGrades$StudentCount[i]
}

# Make sure that the MarkedRedo and totalRedo columns are the same.
identical(students$MarkedRedo, students$totalRedo)

# In the students data.frame, add columns to include the course name and plan for each redo course
coursecolumnCount = max(students$MarkedRedo)
coursecolumnNames = apply(expand.grid(c("course", "plan"), 1:coursecolumnCount), 1, paste0, collapse = "_")
students[,coursecolumnNames] = ""

# Load the course names and plans into the new columns in the students table
for(i in 1:nrow(RevisedGrades)){
  curCourse = RevisedGrades$Course[i]
  curPlan = RevisedGrades$ThePlan[i]
  if(curPlan == "Definitely Summer"){
    curCourse = RevisedGrades$SummerCourse[i]
  }
  curID = RevisedGrades$Student.Number[i]
  curCount = RevisedGrades$StudentCount[i]
  
  students[students$Student.Number == curID,paste0("course_",curCount)] = curCourse
  students[students$Student.Number == curID,paste0("plan_",curCount)] = curPlan
}

# Get the subset of students who need letters sent and select just the relevant columns
studentLetters = students[students$MarkedRedo > 0,]
studentLetters = studentLetters[,c("Student.Number", "Student", "Grade_Level", coursecolumnNames)]
rownames(studentLetters) = NULL

# Replace plans with terminology that makes more sense to parents
planColumns = paste0("plan_", 1:coursecolumnCount)
for(i in planColumns){
  studentLetters[,i] = gsub(pattern = "Definitely Summer", replacement = "Summer School", x = studentLetters[,i], fixed = T)
  studentLetters[,i] = gsub(pattern = "Summer on Plato", replacement = "Summer School online", x = studentLetters[,i], fixed = T)
  studentLetters[,i] = gsub(pattern = "Retake on Plato", replacement = "Retake online", x = studentLetters[,i], fixed = T)
}

# Output the data for making student letters
write.csv(x = studentLetters, file = paste0(OutFolder, "summer school mail merge data.csv"))


