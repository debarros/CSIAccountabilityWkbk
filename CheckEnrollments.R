# CheckEnrollments.R

# The purpose of this script is to identify errors in scheduling

# get the powerschool student and enrollment data
powerschool = powerschoolraw
cc = cc.raw
cc = cc[cc$TermID > 0,]
colnames(cc)[colnames(cc) == "[01]Student_Number"] = "StuNum"


#-----------------------------------------------------------#
#### check for enrollments with no course or course code ####
#-----------------------------------------------------------#

# These are called Courseless Enrollments
if(sum(is.na(cc$`[02]course_name`)) > 0){
  print(cc[is.na(cc$`[02]course_name`),])
} else {
  print("All enrollments have course names.  yay!")
}

if(sum(is.na(cc$Course_Number)) > 0){
  print(cc[is.na(cc$Course_Number),])
} else {
  print("All enrollments have course numbers.  yay!")
}



#--------------------------------------------#
#### check for courses with no department ####
#--------------------------------------------#


cc = DFna.to.empty(cc)
cc$dept = FullAlignment$Subject[match(x = cc$`[02]course_name`, FullAlignment$Course)] #add dept alignments

if(sum(is.na(cc$dept)) > 0) {
  courses.nodept = unique(cc$`[02]course_name`[is.na(cc$dept)])
  print("The following courses have no department in the alignment table: ")
  print(courses.nodept)
} else {
  print("All courses have departments.  Yay!")
}

depts = unique(cc$dept) #get a list of the departments 
depts = depts[!is.na(depts)]



#--------------------------------------------#
#### Make sure students have core courses ####
#--------------------------------------------#


cc$StudentID = powerschool$ID[match(cc$StuNum, powerschool$student_number)]
student.frame = data.frame(StudentNumber = unique(cc$StuNum), stringsAsFactors = F)           # set up a data.frame of students who have classes
student.frame$Last.Name = cc$`[01]last_name`[match(student.frame$StudentNumber, cc$StuNum)]   # add last names
student.frame$First.Name = cc$`[01]first_name`[match(student.frame$StudentNumber, cc$StuNum)] # add first names
student.frame$GradeLevel = powerschool$grade_level[match(student.frame$StudentNumber, powerschool$student_number)] # add grade levels
student.frame[,depts] = NA_integer_                                                           # add columns to hold #classes each student has in each dept
for(i in 1:nrow(student.frame)){                                                              # calculate #classes each student has in each dept
  for(j in depts){
    student.frame[i,j] = sum(cc$StuNum == student.frame$StudentNumber[i] & cc$dept == j)
  }
}

# determine which students have no classes in at least one core subject 
student.frame$missingCore = apply(X = student.frame[,c("ELA", "SSt", "Math", "Science")], MARGIN = 1, FUN = function(x){any(x == 0)})


# output the file of students who are missing at least one core subject
cols2use = c("StudentNumber","Last.Name","First.Name","ELA", "SSt", "Math", "Science")
write.csv(student.frame[student.frame$missingCore,cols2use], file = paste0(OutFolder,"missingCoreClasses.csv"))


# determine the enrollment status of students.  Note that 0 means Active, 2 means Transferred Out, and 3 means Graduated.
student.frame$status = powerschool$Enroll_Status[match(student.frame$StudentNumber, powerschool$student_number)]
summary(as.factor(student.frame$status))

# export the file of students who are not active but still have class enrollments
if(sum(student.frame$status) > 0){
  cols2use = c("StudentNumber","Last.Name","First.Name")
  View(cc[cc$StuNum %in% student.frame$StudentID[student.frame$status != 0],])
  write.csv(student.frame[student.frame$status > 0,cols2use], file = paste0(OutFolder, "exited_students_who_still_have_classes.csv"))
  print("There are inactive students who are still enrolled in sections.  See the output file.")
} else {
  print("Yay!  There are no inactive students who are still enrolled in sections.")
}







# How about students who only have advisory and nothing else?
student.frame$totalEnrollments = 0
for(i in 1:nrow(student.frame)){
  student.frame$totalEnrollments[i] = sum(cc$StuNum == student.frame$StudentNumber[i])
}




write.csv(x = student.frame[student.frame$totalEnrollments < 4,], file = paste0(OutFolder,"students with very few classes.csv"))

summary(as.factor(student.frame$totalEnrollments))



#-----------------------------------------#
##### Let's check for bad section info ####
#-----------------------------------------#

sections = sections[sections$TermID >= schoolYear("termID"),]
sections$delocated = F
sections$delocated[is.na(sections$Room)] = T
if(sum(sections$delocated) > 0){
  write.csv(x = sections[sections$delocated,], file = paste0(OutFolder, "delocated sections.csv"))
} else {
  print("Yay! no delocated sections!")
}


sections[is.na(sections$Course_Number),]
sections[is.na(sections$`[05]lastfirst`),]
sections[is.na(sections$Section_Number),]

sections$timeless = F
sections$timeless[is.na(sections$Expression)] = T
if(sum(sections$timeless) > 0){
  write.csv(x = sections[sections$timeless,], file = paste0(OutFolder, "timeless sections.csv"))
} else {
  print("Yay! no timeless sections!")
}


sections[is.na(sections$`[02]Course_Name`),]





# Check for ghost enrollments (enrollments in sections that were deleted)

cc$ghost = F
for(i in 1:nrow(cc)){
  sectID = cc$SectionID[i]
  cc$ghost[i] = !(sectID %in% sections$ID)
}

if(sum(cc$ghost) > 0){
  write.csv(x = cc[cc$ghost,], file = paste0(OutFolder, "ghost enrollments.csv"))
} else {
  print("Yay!  No ghost enrollments.")
}



# Still need to find a way to test for these:
# Ex-teacher assignments - when a section is assigned to a teacher who no longer works at GTH
# Mallocated sections - when a section is assigned to a room that does not exist




#-----------------------------------------------------#
#### Which students are missing the bridge period? ####
#-----------------------------------------------------#


student.frame$Has1st = F
student.frame$Has8th = F
for(i in 1:nrow(cc)){
  curExp = cc$expression[i]
  curID = cc$StuNum[i]
  if(curExp == "1(A)"){
    student.frame$Has1st[student.frame$StudentNumber == curID] = T
  }
  if(curExp == "8(A)"){
    student.frame$Has8th[student.frame$StudentNumber == curID] = T
  }
}

student.frame$BadBridge = !(student.frame$Has1st & student.frame$Has8th)

badBridge = student.frame[student.frame$BadBridge,]

badBridge = badBridge[badBridge$GradeLevel != 6,]

write.csv(x = badBridge, file = paste0(OutFolder, "missing bridge.csv"))




#---------------------------------------#
#### Which sections have no teacher? ####
#---------------------------------------#





#----------------------------------------#
#### Which sections have no students? ####
#----------------------------------------#


sections$hasStudents = F
for(thisSectID in unique(cc$SectionID)){
  sections$hasStudents[sections$ID == thisSectID] = T
}
if(sum(!sections$hasStudents) > 0){
  write.csv(x = sections[!sections$hasStudents,], file = paste0(OutFolder, "empty sections.csv"))
} else {
  print("Yay!  No empty sections")
}




#--------------------------------------------------#
#### Which students have Lit Lab but not Lit 9? ####
#--------------------------------------------------#


cc.litlab = cc[cc$`[02]course_name` == "Literature 9 Lab",]
cc.litlab$MissingLit9 = T
for(thisStuRow in 1:nrow(cc.litlab)){
  thisStu = cc.litlab$StuNum[thisStuRow]
  cc.thisStu = cc[cc$StuNum == thisStu & cc$`[02]course_name` == "Literature 9- 1 credit",]
  if(nrow(cc.thisStu) > 0){
    cc.litlab$MissingLit9[thisStuRow] = F
  }
}
cc.litlab.missing = cc.litlab[cc.litlab$MissingLit9,]
if(nrow(cc.litlab.missing) > 0){
  print("Uh oh, students are missing Lit 9.")
  write.csv(x = cc.litlab.missing, file = paste0(OutFolder, "LitLabStudentsMissingLit9.csv"))
} else {
  print("Yay!  All lit lab students have lit 9.")
}

