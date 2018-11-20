# Attendance.R

#---------------------------------------------------#
#### Find seniors who cut class in the afternoon ####
#---------------------------------------------------#

today = read.xlsx(file.choose())  # read in an export of the attendance table for just one day
powerschool = powerschoolraw
cc = cc.raw

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



#----------------------------------------------------#
#### Find attendance records after student exited ####
#----------------------------------------------------#
thisYear = attendance
thisYear$exitDate = powerschool$ExitDate[match(
  thisYear$`[1]Student_Number`, powerschool$student_number)]          # Add the date the student exited the school
thisYear.bad = thisYear[thisYear$Att_Date > thisYear$exitDate,]       # Limit to attendance records dated after exit
if(nrow(thisYear.bad) > 0){
  # Remove unnecessary columns
  cols2use = c("[1]LastFirst", "[1]Student_Number", "Att_Date", "exitDate", "CCID", "ID")
  thisYear.bad = thisYear.bad[,cols2use]   
  thisYear.bad$SectionEntry = cc$DateEnrolled[match(thisYear.bad$CCID, cc$ID)]             # Add the date the student entered the section
  thisYear.bad$SectionExit = cc$DateLeft[match(thisYear.bad$CCID, cc$ID)]                  # Add the date the student exited the section
  # Add the internal powerschool ID for the student
  thisYear.bad$StudentID = powerschool$ID[match(
    thisYear.bad$`[1]Student_Number`, powerschool$student_number)] 
  thisYear.bad$Action = ""                                                                 # Initialize the Action field
  for(i in 1:nrow(thisYear.bad)){
    act = "Delete Attendance.  "                                                           # Mark the attendance record for deletion
    if(thisYear.bad$SectionEntry[i] > thisYear.bad$exitDate[i]){                           # If student entered section after leaving school,
      act = paste0(act, "Delete from cc table.  ")                                         # Mark the section enrollment record for deletion
    } else {
      if(cc$TermID[cc$ID == thisYear.bad$CCID[i]] > 0){                                    # If the section enrollment is active
        act = paste0(act, "Exit the student from the section.  ")                          # Mark that it should be exited
      } else {
        if(thisYear.bad$SectionExit[i] > thisYear.bad$exitDate[i]){                        # If section enrollment ends after the student left,
          act = paste0(act, "Change the section exit date.  ")                             # mark that the section exit date should be changed
        } # /if
      } # /if-else
    } # /if-else section enrollment began after the student transfered out of GTH
    thisYear.bad$Action[i] = act
  } # /for
  write.csv(x = thisYear.bad, file = paste0(OutFolder, "cc deletions.csv"))
  print(paste0("Open the file and follow the instructions for each record.  ",
               "Note that the ID column is the ID field value from the Attendance table.  ", 
               "Use DDA to delete records."))
} else {
  print("There are no attendance records to correct.  Yay!")
} # /if-else there are attendance records to correct




#----------------------------------------#
#### Calculate Attendance Risk Scores ####
#----------------------------------------#

# Make a copy of the powerschool export that includes only current students
powerschool = powerschoolraw[powerschoolraw$student_number %in% currentStudents$Student_Number,]

attendance = attendance[attendance$Att_Mode_Code == "ATT_ModeDaily",]                            # limit to daily attendance
attendance$Desc = attendCodes$Description[match(attendance$Attendance_CodeID, attendCodes$ID)]   # pull in the description
attendance$code = attendCodes$Att_Code[match(attendance$Attendance_CodeID, attendCodes$ID)]      # pull in the code

# Get lists of attendance descriptions/statuses
statuses = unique(attendCodes$Description)
absentcodes = intersect(statuses, 
                        c("Absence Unexcused", "Absence Excused", "Suspension", "Expelled", "Night School Absent", 
                          "ISS Absent unexcused", "ISS Absent Excused", "Night School Absent Excused"))
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
  10 * (powerschool$`Absence Unexcused` + powerschool$`ISS Absent unexcused` + powerschool$`Night School Absent`) +    # 10 pts per AU
  5 * powerschool$`Tardy Unexcused` +                                                                                  #  5 pts per TU
  4 * (powerschool$`Absence Excused` + powerschool$`ISS Absent Excused` + powerschool$`Night School Absent Excused`) + #  4 pts per AE
  1 * (powerschool$`ISS Tardy Excused` + powerschool$`Tardy Excused`)                                                  #  1 pt  per TE

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
attRiskOutput = powerschool[
  powerschool$attendanceRiskz > 0.5, 
  c("student_number", "lastfirst", "grade_level", "totalAbsences", "totalAU", "attendanceRiskCategory")]
attRiskOutput = attRiskOutput[order(attRiskOutput$attendanceRiskCategory, attRiskOutput$totalAbsences, decreasing = T),]
sum(attRiskOutput$attendanceRiskCategory > 0)
write.csv(x = attRiskOutput, file = paste0(OutFolder, "attendanceRisk.csv"))






#-----------------------------------------------------------#
#### Find meeting attendance for nonexistent enrollments ####
#-----------------------------------------------------------#

cc = cc.raw
sum(is.na(attendance$CCID)) # should be 0

att.mtg = attendance[attendance$CCID != 0,]
att.day = attendance[attendance$CCID == 0,]

att.mtg$CourseName = cc$`[02]course_name`[match(att.mtg$CCID, cc$ID)] # for meeting attendance, get the course name

sum(is.na(cc$`[02]course_name`)) # should be 0

badenr = setdiff(att.mtg$CCID, cc$ID)           # enrollments for which there is attendance but nothing in the cc table

att.badenr = att.mtg[att.mtg$CCID %in% badenr,] # get all the mtg attendance for enrollments that have nothing in the cc table
unique(att.badenr$`[1]LastFirst`)               # check these students to see if they actually attended gth
unique(att.badenr$StudentID)                    # if they didn't, find the attendance records with these student IDs and delete them
att.badenr$ID                                   # this is the ID for all the mtg attendance records that have no associate course



#------------------------------------------------------------#
#### Find mismatches between daily and meeting attendance ####
#------------------------------------------------------------#

cc = cc.raw
sum(is.na(attendance$CCID)) # should be 0

statuses = unique(attendCodes$Description)
absentcodes = intersect(statuses, 
                        c("Absence Unexcused", "Absence Excused", "Suspension", "Expelled", "Night School Absent", 
                          "ISS Absent unexcused", "ISS Absent Excused", "Night School Absent Excused"))
AUcodes = intersect(statuses, c("Absence Unexcused", "Night School Absent", "ISS Absent unexcused"))
presentcodes = c("Present", "Present ISS", "Night School Present")

attendance$Desc = attendCodes$Description[match(attendance$Attendance_CodeID, attendCodes$ID)]   # pull in the description
attendance$code = attendCodes$Att_Code[match(attendance$Attendance_CodeID, attendCodes$ID)]      # pull in the code
attendance$StudDate = paste0(attendance$`[1]Student_Number`, " - " ,attendance$Att_Date)

att.mtg = attendance[attendance$CCID != 0,]
att.day = attendance[attendance$CCID == 0,]

att.mtg$CourseName = cc$`[02]course_name`[match(att.mtg$CCID, cc$ID)]              # for meeting attendance, get the course name
att.mtg$Teacher = cc$`[05]lastfirst`[match(att.mtg$CCID, cc$ID)]                   # for meeting attendance, get the course name
att.mtg$period = periodCodes$Abbreviation[match(att.mtg$PeriodID, periodCodes$ID)]
sum(is.na(cc$`[02]course_name`))                                                   # should be 0
att.mtg = att.mtg[att.mtg$CourseName != "Lunch",]

students = unique(attendance$`[1]Student_Number`)
dates = unique(attendance$Att_Date)

att.day.pres = att.day[att.day$Desc %in% presentcodes,]
att.day.pres$mtgPres = 0
att.day.pres$mtgAbs = 0
for(i in 1:nrow(att.day.pres)){
  StudDate = att.day.pres$StudDate[i]
  att.day.pres$mtgPres[i] = sum(att.mtg$StudDate == StudDate & att.mtg$Desc %in% presentcodes)
  att.day.pres$mtgAbs[i] = sum(att.mtg$StudDate == StudDate & att.mtg$Desc %in% absentcodes)
}

table(att.day.pres[,c("mtgPres", "mtgAbs")])


# The following line shows all of the situations where a student was marked present for the day,
# has no periods where is was marked present, and has at least one period where he was marked absent
att.day.pres[att.day.pres$mtgPres == 0 & att.day.pres$mtgAbs > 0, c("StudDate", "[1]LastFirst")]






att.day.abs = att.day[att.day$Desc %in% absentcodes,]
att.day.abs$mtgPres = 0
att.day.abs$mtgAbs = 0
for(i in 1:nrow(att.day.abs)){
  StudDate = att.day.abs$StudDate[i]
  att.day.abs$mtgPres[i] = sum(att.mtg$StudDate == StudDate & att.mtg$Desc %in% presentcodes)
  att.day.abs$mtgAbs[i] = sum(att.mtg$StudDate == StudDate & att.mtg$Desc %in% absentcodes)
}

table(att.day.abs[,c("mtgPres", "mtgAbs")])

att.day.abs.prob = att.day.abs[att.day.abs$mtgPres > 0, c("StudDate", "[1]LastFirst", "Att_Date")]
nrow(att.day.abs.prob)
x = summary(factor(att.day.abs.prob$`[1]LastFirst`))
summary(x)
summary(factor(att.day.abs.prob$Att_Date))
summary(summary(factor(att.day.abs.prob$Att_Date)))


att.mtg$problem = F
for(i in 1:nrow(att.day.abs.prob)){
  StudDate = att.day.abs.prob$StudDate[i]
  att.mtg$problem[att.mtg$StudDate == StudDate & att.mtg$Desc %in% presentcodes] = T
}

att.mtg.prob = att.mtg[att.mtg$problem,c("[1]LastFirst", "[1]Student_Number", "Att_Date", "Desc", "CourseName", "Teacher", "period")]
colnames(att.mtg)
att.mtg.prob$period = factor(att.mtg.prob$period, levels = sort(unique(att.mtg.prob$period)))
att.mtg.prob$Teacher = factor(att.mtg.prob$Teacher, levels = sort(unique(att.mtg.prob$Teacher)))
summary(factor(att.mtg.prob$Teacher))

summary(att.mtg.prob$period)

table(att.mtg.prob[,c("Teacher", "period")])


write.csv(att.day.abs.prob, paste0(OutFolder, "Attendance issues.csv"))


#-----------------------------------------#
#### List students out on a given date ####
#-----------------------------------------#

att.oneday = read.xlsx(xlsxFile = PSLocation, sheet = "OneDayAttendance")

att.oneday$Att_Date = xlDate(att.oneday$Att_Date)

att.oneday = att.oneday[att.oneday$Att_Date == as.Date("2018-10-17"),] # enter the desired date
att.oneday = att.oneday[att.oneday$Att_Mode_Code == "ATT_ModeDaily",]


att.oneday$Att_Code = attendCodes$Att_Code[match(att.oneday$Attendance_CodeID, attendCodes$ID)]

att.oneday[,c("[1]LastFirst", "[1]Student_Number", "Att_Date", "Att_Code")]
