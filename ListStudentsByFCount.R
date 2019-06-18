# ListStudentsByFCount.R


# This script uses the unstored grades and cc table
# It is used to find all students who have some specific number of Fs
# It still needs some work

#-----------------#
#### Load Data ####
#-----------------#

allGrades = unstoredGrades
x = colnames(allGrades)
x[x == "Class"] = "Grade.Level"
colnames(allGrades) = x
# allGrades = allGrades[allGrades$Course != "Exploring Algebra with Technology",]

cc = cc.raw[cc.raw$DateLeft > Sys.Date(),]
cc = cc[cc$`[02]course_name` != "Career Internship",]




#---------------------------#
#### Organize by Student ####
#---------------------------#


Students = allGrades[!duplicated(allGrades$Student.Number),c("Student.Number", "Student", "Grade.Level")]
Students$F.count = 0
Students$F.courses = ""
for(i in 1:nrow(Students)){
  theseGrades = allGrades[allGrades$Student.Number == Students$Student.Number[i],]
  theseFs = theseGrades[theseGrades$Grade == "F",]
  Students$F.count[i] = nrow(theseFs)
  Students$F.courses[i] = paste0(theseFs$Course, collapse = " || ")
}


#----------------#
#### Just 1 F ####
#----------------#

Students.1F = Students[Students$F.count == 1,]



#--------------------#
#### Add Schedule ####
#--------------------#

# This part does not work

Students.1F[,c("Adv", paste0("Period",1:8))] = ""

for(i in 1:nrow(Students.1F)){
  theseCourses = cc[cc$`[01]Student_Number` == Students.1F$Student.Number[i],]
  Students.1F$Adv[i] = theseCourses$`[05]lastfirst`[theseCourses$expression == "9(A)"]
  for(j in 1:8){
    expr = paste0(j,"(A)")
    if(expr %in% theseCourses$expression){
      Students.1F[i,expr] = theseCourses$`[05]lastfirst`[theseCourses$expression == expr]  
    }
  }
}



#--------------#
#### Export ####
#--------------#

write.csv(x = Students.1F, file = paste0(OutFolder, "StudentsWith1F.csv"))



