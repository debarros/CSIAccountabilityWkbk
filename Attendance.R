# Attendance.R

today = read.xlsx(file.choose())
powerschool = powerschoolraw
cc = cc.raw

# Just seniors who cut class in the afternoon ####
today.mtg = today[today$Att_Mode_Code == "ATT_ModeMeeting",]
attendCodes = DFna.to.empty(attendCodes)
today.mtg$Att_Code = attendCodes$Att_Code[match(today.mtg$Attendance_CodeID, attendCodes$ID)]
today.mtg$Grade = Workbook$Grade.[match(today.mtg$`[1]Student_Number`, Workbook$Local.ID)]
today.mtg = today.mtg[!is.na(today.mtg$Grade),]
today.mtg = today.mtg[today.mtg$Grade == 12,]
today.mtg = today.mtg[today.mtg$Att_Code == "AU",]
today.mtg$Period = periodCodes$Abbreviation[match(today.mtg$PeriodID, periodCodes$ID)]

today.mtg = today.mtg[today.mtg$Period != "HR",]
today.mtg$Period = as.integer(today.mtg$Period)
today.mtg = today.mtg[today.mtg$Period > 4,]


today.mtg = today.mtg[,c("[1]LastFirst", "[1]Student_Number", "Att_Code", "Grade", "Period")]

today.mtg = today.mtg[order(today.mtg$`[1]LastFirst`, today.mtg$Period),]

write.csv(x = today.mtg, file = paste0(OutFolder, "absences.csv"))


# find situations where a student has attendance records after he exited ####
# thisYear = today
thisYear = read.xlsx(file.choose())
thisYear$Att_Date = xlDate(thisYear$Att_Date)
thisYear$exitDate = powerschool$ExitDate[match(thisYear$`[1]Student_Number`, powerschool$student_number)]
thisYear.bad = thisYear[thisYear$Att_Date > thisYear$exitDate,]
thisYear.bad = thisYear.bad[,c("[1]LastFirst", "[1]Student_Number", "Att_Date", "exitDate", "CCID")]
thisYear.bad$SectionEntry = cc$DateEnrolled[match(thisYear.bad$CCID, cc$ID)]
thisYear.bad$Action = "Exit"
thisYear.bad$Action[thisYear.bad$SectionEntry > thisYear.bad$exitDate] = "Delete"

write.csv(x = thisYear.bad, file = paste0(OutFolder, "cc deletions.csv"))






#----------------------------------------#
#### Calculate Attendance Risk Scores ####
#----------------------------------------#

# Make a copy of the powerschool export that includes only current students
powerschool = powerschoolraw[powerschoolraw$student_number %in% currentStudents$Student_Number,]

attendance = attendance[attendance$Att_Mode_Code == "ATT_ModeDaily",]                          # limit to daily attendance
attendance$Desc = attendCodes$Description[match(attendance$Attendance_CodeID, attendCodes$ID)] # pull in the description
attendance$code = attendCodes$Att_Code[match(attendance$Attendance_CodeID, attendCodes$ID)]    # pull in the code

# Get lists of attendance descriptions/statuses
statuses = unique(attendCodes$Description)
absentcodes = intersect(statuses, c("Absence Unexcused", "Absence Excused", "Suspension", "Expelled", "Night School Absent", "ISS Absent unexcused", "ISS Absent Excused", "Night School Absent Excused"))
AUcodes = intersect(statuses, c("Absence Unexcused", "Night School Absent", "ISS Absent unexcused"))
powerschool[,statuses] = 0 # Add columns for the various statuses

# Count how many times each student has each kind of attendance code
# It would be better if this could be done without loops
for(i in 1:nrow(powerschool)){
  for(j in statuses){
    powerschool[i,j] = sum(attendance$`[1]Student_Number` == powerschool$student_number[i] & attendance$Desc == j)
  } # /for each status
} # /for each student
powerschool$totalAbsences = apply(powerschool[,absentcodes], 1, sum) # add a column for total absences
powerschool$totalAU = apply(powerschool[,AUcodes], 1, sum)           # add a column for total unexcused absences


# Calculate the attendance risk
powerschool$attendanceRisk = 
  10 * (powerschool$`Absence Unexcused` + powerschool$`ISS Absent unexcused` + powerschool$`Night School Absent`) +    # 10 pts per unexcused absence
  5 * powerschool$`Tardy Unexcused` +                                                                                  #  5 pts per unexcused tardy
  4 * (powerschool$`Absence Excused` + powerschool$`ISS Absent Excused` + powerschool$`Night School Absent Excused`) + #  4 pts per excused absence
  1 * (powerschool$`ISS Tardy Excused` + powerschool$`Tardy Excused`)                                                  #  1 pt  per excused tardy

# Take a look at the summarized data
hist(powerschool$attendanceRisk)
summary(powerschool$attendanceRisk)
sum(powerschool$attendanceRisk > mean(powerschool$attendanceRisk))

# Calculate the attendace risk z score and category
powerschool$attendanceRiskz = (powerschool$attendanceRisk - mean(powerschool$attendanceRisk))/ sd(powerschool$attendanceRisk)
powerschool$attendanceRiskCategory = floor(powerschool$attendanceRiskz)

# Take a look at summarized data
summary(powerschool$attendanceRiskCategory)
table(powerschool$attendanceRiskCategory)
sum(powerschool$attendanceRiskCategory > 1)
sum(powerschool$attendanceRiskCategory > 0)
sum(powerschool$attendanceRiskCategory > -1)
sum(powerschool$attendanceRiskz >= .5)

# Apply labels to the attendance risk categories
powerschool$attendanceRiskLabel = factor(x = "Low", levels = c("Low", "Moderate", "High"), labels = c("Low", "Moderate", "High"))
powerschool$attendanceRiskLabel[powerschool$attendanceRiskCategory > -1] = "Moderate"
powerschool$attendanceRiskLabel[powerschool$attendanceRiskCategory > 1] = "High"

# Generate output.  Include students whose risk z score is above 0.5
# It would be nice to output this as an excel workbook with some formatting
attRiskOutput = powerschool[powerschool$attendanceRiskz > 0.5, c("student_number", "lastfirst", "grade_level", "totalAbsences", "totalAU", "attendanceRiskCategory")]
attRiskOutput = attRiskOutput[order(attRiskOutput$attendanceRiskCategory, attRiskOutput$totalAbsences, decreasing = T),]
sum(attRiskOutput$attendanceRiskCategory > 0)
write.csv(x = attRiskOutput, file = paste0(OutFolder, "attendanceRisk.csv"))
