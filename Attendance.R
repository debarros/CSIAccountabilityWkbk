# Attendance.R

today = read.xlsx(file.choose())
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
