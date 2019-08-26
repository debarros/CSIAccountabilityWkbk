# SummerSchool.R

#----------------#
#### Load data ###
#----------------#

# This requires the functions.R file be sourced and the unstoredgrades be loaded
regentsScores = read.xlsx(xlsxFile = PSLocation, sheet = "Regents long")
grades = unstoredGrades
courses = read.xlsx(xlsxFile = PSLocation, sheet = "Summer Courses")


grades = SummerSchool1(grades, courses, regentsScores) # Add additional information and select a plan for each row in the grades data.frame
students = SummerSchool2(grades) # Set up a data.frame of just students

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
write.csv(x = SummerGrades, file = paste0(OutFolder, "SummerGrades table.csv"), row.names = F)

#--------------------------#
#### Manual Adjustments ####
#--------------------------#

# At this point, go through the file and fix the situations in where students still have "Possibly Summer".


#------------------------#
#### Section Planning ####
#------------------------#


# Read in the updated table of grades that are not passing
RevisedGrades = read.csv(file = paste0(OutFolder, "SummerGrades table.csv"), stringsAsFactors = F)

# Get a set of unique summer courses and remove those not being offered
SummerCourses = SummerSchool4(courses, RevisedGrades)
FinalPlans = SummerSchool6(RevisedGrades, SummerCourses) # adjust plan for students in underenrolled courses


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
students = SummerSchool5(students, FinalPlans)

# Create a counter in the FinalPlans data.frame that indicates which record it is for that particular student
FinalPlans$StudentCount = 0
students$MarkedRedo = 0
for(i in 1:nrow(FinalPlans)){
  curID = FinalPlans$Student.Number[i]
  FinalPlans$StudentCount[i] = students$MarkedRedo[students$Student.Number == curID] + 1
  students$MarkedRedo[students$Student.Number == curID] = FinalPlans$StudentCount[i]
}

# Make sure that the MarkedRedo and totalRedo columns are the same.
identical(students$MarkedRedo, students$totalRedo)

# In the students data.frame, add columns to include the course name and plan for each redo course
coursecolumnCount = max(students$MarkedRedo)
coursecolumnNames = apply(expand.grid(c("course", "plan"), 1:coursecolumnCount), 1, paste0, collapse = "_")
students[,coursecolumnNames] = ""

# Load the course names and plans into the new columns in the students table
for(i in 1:nrow(FinalPlans)){
  curCourse = FinalPlans$Course[i]
  curPlan = FinalPlans$ThePlan[i]
  if(curPlan == "Definitely Summer"){
    curCourse = FinalPlans$SummerCourse[i]
  }
  curID = FinalPlans$Student.Number[i]
  curCount = FinalPlans$StudentCount[i]
  
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

# See what sections need to be adjacent to each other in the schedule
Pairings.all$V1.sectCount = SummerCourses$SectionCount[match(Pairings.all$V1, SummerCourses$Equivalent.Summer.Course)]
Pairings.all$V2.sectCount = SummerCourses$SectionCount[match(Pairings.all$V2, SummerCourses$Equivalent.Summer.Course)]
Pairings.all$KeepAdjacent = Pairings.all$V1.sectCount + Pairings.all$V2.sectCount == 2


#-------------------------#
#### Schedule Planning ####
#-------------------------#

# Go look at the output and decide who is teaching what and during which periods


#------------------------------------#
#### Placing students in sections ####
#------------------------------------#


sectionTable = read.xlsx(xlsxFile = PSLocation, sheet = "Summer Sections")

Roster1 = rep(NA_character_, 12)
Roster = list(rep(Roster1, nrow(sectionTable)))
Roster = vector(mode = "list", length = nrow(sectionTable))
for(i in 1:nrow(sectionTable)){ # change this to lapply
  Roster[[i]] = Roster1
}

sectionTable$Roster = Roster
sectionTable$RosterNames = Roster




SummerEnrollments = FinalPlans[FinalPlans$ThePlan == "Definitely Summer",c("Student.Number", "Student", "SummerCourse")]

# Check for courses in summer enrollments but not the section table, and vice versa
setdiff(unique(SummerEnrollments$SummerCourse), unique(sectionTable$Course))
setdiff(unique(sectionTable$Course), unique(SummerEnrollments$SummerCourse))


# Identify students who have 1 singleton and 1 non singleton
SummerEnrollments$CourseIsSingleton = F
for (i in 1:nrow(SummerEnrollments)){
  curCourse = SummerEnrollments$SummerCourse[i]
  curCount = SummerCourses$SectionCount[SummerCourses$Equivalent.Summer.Course == curCourse]
  SummerEnrollments$CourseIsSingleton[i] = curCount == 1
}

SummerEnrollments$Priority = 2
for (i in 1:nrow(SummerEnrollments)){
  curID = SummerEnrollments$Student.Number[i]
  curSingletons = SummerEnrollments$CourseIsSingleton[SummerEnrollments$Student.Number == curID]
  curCourseCount = length(curSingletons)
  if(mean(curSingletons) == 0.5){
    SummerEnrollments$Priority[i] = 3
  } else if(curCourseCount == 1){
    SummerEnrollments$Priority[i] = 1
  }
}


SummerEnrollments = SummerEnrollments[order(SummerEnrollments$Student.Number),]
SummerEnrollments = SummerEnrollments[order(SummerEnrollments$CourseIsSingleton, SummerEnrollments$Priority, decreasing = T),]
rownames(SummerEnrollments) = NULL
SummerEnrollments$ManualAdjustment = F





students[,paste0("HasP", 1:3)] = F

# Assign students to sections

sectionTable = sectionTable[order(sectionTable$Period == 2, sectionTable$Period == 1, decreasing = T),]
rownames(sectionTable) = NULL
sectionTable$Preference = 1:nrow(sectionTable)

AssignmentList = list("SummerEnrollments" = SummerEnrollments, "sectionTable" = sectionTable, "students" = students)
AssignmentList = Summer.AssignStudents(AssignmentList)

SummerEnrollments = AssignmentList$SummerEnrollments
sectionTable = AssignmentList$sectionTable
students = AssignmentList$students

# max class size is 12


# Do any students need manual adjustment?
sum(SummerEnrollments$ManualAdjustment)
SummerEnrollments[SummerEnrollments$ManualAdjustment,]

SummerEnrollments[SummerEnrollments$Student.Number == 161710433,]

# Do any students have P1 and P3 but not P2?
sum(students$HasP1 & students$HasP3 & !students$HasP2)
splitStudentIDs = students$Student.Number[students$HasP1 & students$HasP3 & !students$HasP2]
SummerEnrollments[SummerEnrollments$Student.Number %in% splitStudentIDs,]

Pairings.all


# The next line should also add student grade levels
StudentSchedules = SummerEnrollments[!duplicated(SummerEnrollments$Student.Number), c("Student.Number", "Student")]
StudentSchedules[,paste0("Period",1:3,"Course")] = ""
StudentSchedules[,paste0("Period",1:3,"Teacher")] = ""
row.names(StudentSchedules) = NULL


for(curSect in 1:nrow(sectionTable)){
  print(paste0("section row:", curSect))
  curTeach = sectionTable$Teacher[curSect]
  curCourse = sectionTable$Course[curSect]
  curPeriod = sectionTable$Period[curSect]
  idRoster = sectionTable$Roster[curSect][[1]]
  nameRoster = sectionTable$RosterNames[curSect][[1]]
  for(curStu in 1:length(idRoster)){
    curID = idRoster[curStu]
    print(paste0("current student ID: ",curID))
    if(!is.na(curID)){
      curName = nameRoster[curStu]
      StudentSchedules[StudentSchedules$Student.Number == curID,paste0("Period", curPeriod, "Course")] = curCourse
      StudentSchedules[StudentSchedules$Student.Number == curID,paste0("Period", curPeriod, "Teacher")] = curTeach
    }
  }
}


write.csv(x = StudentSchedules, file = paste0(OutFolder, "summer school student schedules mail merge.csv"))

TeacherRosters = as.data.frame(t(sectionTable[,1:3]), stringsAsFactors = F)
TeacherRosters[(nrow(TeacherRosters)+1):(nrow(TeacherRosters)+19),] = ""

for(i in 1:ncol(TeacherRosters)){
  curRoster = sectionTable$RosterNames[[i]]
  curRoster = curRoster[!is.na(curRoster)]
  curCount = length(curRoster)
  TeacherRosters[(1:curCount)+3,i] = curRoster
}

write.csv(x = TeacherRosters, file = paste0(OutFolder, "summer school rosters.csv"))




grades[grades$Student.Number == 181911732,]

