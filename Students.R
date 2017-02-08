#Students.R

#This script finds discrepancies between PowerSchool and the Accountability Workbook in terms of which students are current

#Export a ton of info from the Students table in PS
psStudentsRaw = read.csv("PowerSchoolStudents.csv")

#Get a list of the students who are in both the Workbook and PS
inBoth = Workbook$Local.ID..optional.[which(Workbook$Local.ID..optional. %in% psStudentsRaw$student_number)]

#Get a list of the students from the workbook
InWorkbook = Workbook$Local.ID..optional.[(Workbook$Still.Enrolled. == "yes"), ]

#Get a list of the students from the workbook who are active 
ActiveInWorkbook = Workbook$Local.ID..optional.[(Workbook$Still.Enrolled. == "yes"), ]

#Get a list of the students who are enrolled in PS, appear in the wkbk, but are not enrolled in the wkbk
InPSNotActiveInWkbk = Workbook$Local.ID..optional.[(Workbook$Still.Enrolled. == "yes") &! (Workbook$Local.ID..optional. %in% inBoth), ]

#Get a list of students who are enrolled in PS but nowhere in the wkbk
InJustPS = psStudentsRaw$student_number[which(!(psStudentsRaw$student_number %in% inBoth))]

#Get a list of students who are active in the workbook, appear in PS, but are not enrolled in PS
#write code here

#Get a list of students who are active in the workbook, but are not in PS at all
#write code here

#Compile information into one readable table
#write code here