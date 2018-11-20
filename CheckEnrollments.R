# CheckEnrollments.R

# The purpose of this script is to identify errors in scheduling

# get the powerschool student and enrollment data
powerschool = powerschoolraw
cc = cc.raw
cc = cc[cc$TermID > 0,]


#-----------------------------------------------------------#
#### check for enrollments with no course or course code ####
#-----------------------------------------------------------#

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


cc$StudentID = powerschool$ID[match(cc$`[01]Student_Number`, powerschool$student_number)]

student.frame = data.frame(StudentNumber = unique(cc$`[01]Student_Number`), stringsAsFactors = F) #set up a data.frame of students who have classes
student.frame$Last.Name = cc$`[01]last_name`[match(student.frame$StudentNumber, cc$`[01]Student_Number`)] #add last names
student.frame$First.Name = cc$`[01]first_name`[match(student.frame$StudentNumber, cc$`[01]Student_Number`)] #add first names
student.frame[,depts] = NA_integer_ #add columns to hold the number of classes each student has in each department
for(i in 1:nrow(student.frame)){ #calculate and enter the number of classes each student has in each department
  for(j in depts){
    student.frame[i,j] = sum(cc$`[01]Student_Number` == student.frame$StudentNumber[i] & cc$dept == j)
  }
}

# determine which students have no classes in at least one core subject 
student.frame$missingCore = apply(X = student.frame[,c("ELA", "SSt", "Math", "Science")], MARGIN = 1, FUN = function(x){any(x == 0)})


# output the file of students who are missing at least one core subject
write.csv(student.frame[student.frame$missingCore,c("StudentNumber","Last.Name","First.Name","ELA", "SSt", "Math", "Science")], file = paste0(OutFolder,"missingCoreClasses.csv"))


# determine the enrollment status of students.  Note that 0 means Active, 2 means Transferred Out, and 3 means Graduated.
student.frame$status = powerschool$Enroll_Status[match(student.frame$StudentNumber, powerschool$student_number)]
summary(as.factor(student.frame$status))

# export the file of students who are not active but still have class enrollments
write.csv(student.frame[student.frame$status > 0,c("StudentNumber","Last.Name","First.Name")], file = paste0(OutFolder, "exited_students_who_still_have_classes.csv"))


cc[cc$`[01]Student_Number` %in% student.frame$StudentID[student.frame$status > 0],]



# How about students who only have advisory and nothing else?
student.frame$totalEnrollments = 0
for(i in 1:nrow(student.frame)){
  student.frame$totalEnrollments[i] = sum(cc$`[01]Student_Number` == student.frame$StudentNumber[i])
}


student.frame[student.frame$totalEnrollments == 1,]

summary(as.factor(student.frame$totalEnrollments))
