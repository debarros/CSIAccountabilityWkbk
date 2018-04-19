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
thisYear$exitDate = powerschool$ExitDate[match(thisYear$`[1]Student_Number`, powerschool$student_number)]      # Add the date the student exited the school
thisYear.bad = thisYear[thisYear$Att_Date > thisYear$exitDate,]                                                # Limit to attendance records dated after exit
if(nrow(thisYear.bad) > 0){
  thisYear.bad = thisYear.bad[,c("[1]LastFirst", "[1]Student_Number", "Att_Date", "exitDate", "CCID", "ID")]   # Remove unnecessary columns
  thisYear.bad$SectionEntry = cc$DateEnrolled[match(thisYear.bad$CCID, cc$ID)]                                 # Add the date the student entered the section
  thisYear.bad$SectionExit = cc$DateLeft[match(thisYear.bad$CCID, cc$ID)]                                      # Add the date the student exited the section
  thisYear.bad$StudentID = powerschool$ID[match(thisYear.bad$`[1]Student_Number`, powerschool$student_number)] # Add the internal powerschool ID for the student
  thisYear.bad$Action = ""                                                                                     # Initialize the Action field
  for(i in 1:nrow(thisYear.bad)){
    act = "Delete Attendance.  "                                                                               # Mark the attendance record for deletion
    if(thisYear.bad$SectionEntry[i] > thisYear.bad$exitDate[i]){                                               # If student entered section after leaving school,
      act = paste0(act, "Delete from cc table.  ")                                                             # Mark the section enrollment record for deletion
    } else {
      if(cc$TermID[cc$ID == thisYear.bad$CCID[i]] > 0){                                                        # If the section enrollment is active
        act = paste0(act, "Exit the student from the section.  ")                                              # Mark that it should be exited
      } else {
        if(thisYear.bad$SectionExit[i] > thisYear.bad$exitDate[i]){                                            # If the section enrollment ends after the student left,
          act = paste0(act, "Change the section exit date.  ")                                                 # mark that the section exit date should be changed
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
