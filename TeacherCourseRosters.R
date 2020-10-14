# TeacherCourseRosters.R


cc = cc.raw[cc.raw$SectionID > 0,]

cc$TeacherCourse = paste0(cc$`[05]lastfirst`, " - ", cc$`[02]course_name`)
cc$StuNameID = paste0(cc$`[01]last_name`, ", ", cc$`[01]first_name`, " (", cc$`[01]Student_Number`,")")
cc = cc[order(cc$`[01]last_name`, cc$`[01]first_name`, cc$`[01]Student_Number`),]
rownames(cc) = NULL

TeacherCourseList = unique(cc$TeacherCourse)
TeacherCourseList = TeacherCourseList[order(TeacherCourseList)]

x = 0
for(i in TeacherCourseList){
  x = betterMax(c(x, sum(cc$TeacherCourse == i)))
}


x = matrix(data = "", nrow = x + 3, ncol = length(TeacherCourseList))

x[1,] = TeacherCourseList

for(thisColumn in 1:ncol(x)){
  thisTC = x[1,thisColumn]
  thisrowset = cc[cc$TeacherCourse == thisTC,]
  thisTeacher = thisrowset$`[05]lastfirst`[1]
  thisCourse = thisrowset$`[02]course_name`[1]
  x[2,thisColumn] = thisTeacher
  x[3,thisColumn] = thisCourse
}


thisCC = 10
for(thisCC in 1:nrow(cc)){
  thisStudent = cc$StuNameID[thisCC]
  thisTeacherCourse = cc$TeacherCourse[thisCC]
  thisColumn = which(x[1,] == thisTeacherCourse)
  EmptySpaces = which(x[,thisColumn] == "")
  FirstSpace = EmptySpaces[1]
  x[FirstSpace, thisColumn] = thisStudent
}


write.csv(x = x, file = paste0(OutFolder, "TeacherCourseRosters.csv"))



