#Students.R

#Export a ton of info from the Students table in PS
psStudentsRaw = read.csv("PowerSchoolStudents.csv")

#Limit the data from the workbook to just actual rows
Workbook = Workbookraw[!is.na(Workbookraw$Local.ID..optional.),]

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

