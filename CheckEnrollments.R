# CheckEnrollments.R

# The purpose of this script is to identify errors in scheduling
cc = cc.raw
cc$dept = FullAlignment$Subject[match(x = cc$`[02]course_name`, FullAlignment$Course)] #add dept alignments
sum(is.na(cc$dept)) #check for courses with no subject alignment
depts = unique(cc$dept) #get a list of the departments 
student.frame = data.frame(StudentID = unique(cc$`[01]Student_Number`), stringsAsFactors = F) #set up a data.frame of students who have classes
student.frame$Last.Name = cc$`[01]last_name`[match(student.frame$StudentID, cc$`[01]Student_Number`)] #add last names
student.frame$First.Name = cc$`[01]first_name`[match(student.frame$StudentID, cc$`[01]Student_Number`)] #add first names
student.frame[,depts] = NA_integer_ #add columns to hold the number of classes each student has in each department
for(i in 1:nrow(student.frame)){ #calculate and enter the number of classes each student has in each department
  for(j in depts){
    student.frame[i,j] = sum(cc$`[01]Student_Number` == student.frame$StudentID[i] & cc$dept == j)
  }
}

# determine which students have no classes in at least one core subject 
student.frame$missingCore = apply(X = student.frame[,c("ELA", "SSt", "Math", "Science")], MARGIN = 1, FUN = function(x){any(x == 0)})


# output the file of students who are missing at least one core subject
write.csv(student.frame[student.frame$missingCore,c("StudentID","Last.Name","First.Name","ELA", "SSt", "Math", "Science")], file = "missingCoreClasses.csv")

# get the powerschool student data
powerschool = powerschoolraw

# determine the enrollment status of students.  Note that 0 means Active.
student.frame$status = powerschool$Enroll_Status[match(student.frame$StudentID, powerschool$student_number)]
summary(as.factor(student.frame$status))

# export the file of students who are not active but still have class enrollments
write.csv(student.frame[student.frame$status > 0,c("StudentID","Last.Name","First.Name")], file = "exited_students_who_still_have_classes.csv")
