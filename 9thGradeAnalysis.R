#9thGradeAnalysis.R
cc = cc.raw

# Check the data ####
sum(is.na(currentGrades$`[1]Student_Number`))
sum(is.na(currentGrades$Course_Name))
sum(is.na(currentGrades$Grade))

currentGrades = currentGrades[!is.na(currentGrades$Grade),]

sum(is.na(currentGrades$Teacher_Name))
nagrades = currentGrades[is.na(currentGrades$Teacher_Name),]
View(nagrades)
summary(as.factor(nagrades$Course_Name))
summary(as.factor(nagrades$StoreCode))
summary(as.factor(nagrades$DateStored))
summary(as.factor(nagrades$SchoolName))
summary(as.factor(nagrades$Grade_Level))
summary(as.factor(nagrades$Grade))
summary(as.factor(nagrades$GradeScale_Name))
summary(as.factor(nagrades$`[1]LastFirst`))
sort(unique(nagrades$`[1]LastFirst`))
write.csv(nagrades, paste0(OutFolder, "grades with no teacher.csv"))



# Do other stuff ####

currentGrades$subject = FullAlignment$Subject[match(currentGrades$Course_Name, FullAlignment$Course)]
sum(is.na(currentGrades$subject)) # should be 0
currentGrades = currentGrades[currentGrades$Grade_Level == 9,] # limit to 9th grade
currentGrades$period = cc$expression[match(currentGrades$SectionID, cc$SectionID)]
currentGrades = currentGrades[!is.na(currentGrades$period),] # remove stored grades that were manually entered, not earned at GTH
currentGrades$period = gsub(pattern = "[^[:digit:]]", replacement = "", x = currentGrades$period)
currentGrades$period[currentGrades$period == "9"] = "A"

currentGrades.elaandmath = currentGrades[currentGrades$subject %in% c("ELA", "Math"),]

currentGrades.elaandmath.Fs = currentGrades.elaandmath[currentGrades.elaandmath$Grade == "F",]
currentGrades.elaandmath.Fs = currentGrades.elaandmath.Fs[currentGrades.elaandmath.Fs$StoreCode %in% paste0("Q",1:4),] # limit to quarters, not semesters or exams
currentGrades.elaandmath.Fs$code = paste0(currentGrades.elaandmath.Fs$`[1]Student_Number`, currentGrades.elaandmath.Fs$Course_Name)

View(currentGrades.elaandmath.Fs)
summary(factor(currentGrades.elaandmath.Fs$code))

# Make the table of students to keep an eye on ####
students2watch = currentGrades.elaandmath.Fs[!duplicated(currentGrades.elaandmath.Fs$`[1]Student_Number`),c("[1]Student_Number", "[1]LastFirst")]
rownames(students2watch) = NULL
students2watch$totalFs = 0
students2watch$mathFs = 0
students2watch$elaFs = 0
for(i in 1:nrow(students2watch)){
  id = students2watch$`[1]Student_Number`[i]
  students2watch$totalFs[i] = sum(currentGrades$`[1]Student_Number` == id & currentGrades$Grade == "F")
  students2watch$mathFs[i] = sum(currentGrades$`[1]Student_Number` == id & currentGrades$Grade == "F" & currentGrades$subject == "Math")
  students2watch$elaFs[i] = sum(currentGrades$`[1]Student_Number` == id & currentGrades$Grade == "F" & currentGrades$subject == "ELA")
}

write.csv(x = students2watch, file = paste0(OutFolder, "students.csv"))


# Make the table of sections to keep an eye on ####
sections2watch = currentGrades.elaandmath.Fs[!duplicated(currentGrades.elaandmath.Fs$SectionID),c("Teacher_Name", "Course_Name", "SectionID", "subject", "period")]
sections2watch$totalFs = 0
for(i in 1:nrow(sections2watch)){
  id = sections2watch$SectionID[i]
  sections2watch$totalFs[i] = sum(currentGrades.elaandmath.Fs$SectionID == id)
}
sections2watch = sections2watch[order(sections2watch$subject, sections2watch$Teacher_Name, sections2watch$Course_Name, sections2watch$period),]
rownames(sections2watch) = NULL
write.csv(x = sections2watch, file = paste0(OutFolder, "sections.csv"))


# Make the table of periods to keep an eye on ####
periods2watch = data.frame(period = sort(unique(currentGrades.elaandmath.Fs$period)), stringsAsFactors = F)
periods2watch$totalFs = 0
periods2watch$mathFs = 0
periods2watch$elaFs = 0
for(i in 1:nrow(periods2watch)){
  id = periods2watch$period[i]
  periods2watch$totalFs[i] = sum(currentGrades$period == id & currentGrades$Grade_Level == 9 & currentGrades$Grade == "F")
  periods2watch$mathFs[i] = sum(currentGrades.elaandmath.Fs$period == id & currentGrades.elaandmath.Fs$subject == "Math")
  periods2watch$elaFs[i] = sum(currentGrades.elaandmath.Fs$period == id & currentGrades.elaandmath.Fs$subject == "ELA")
}
periods2watch = periods2watch[order(periods2watch$period),]
rownames(periods2watch) = NULL
write.csv(x = periods2watch, file = paste0(OutFolder, "periods.csv"))



# Make the table of teachers to keep an eye on ####
teachers2watch = data.frame(teacher = sort(unique(currentGrades.elaandmath.Fs$Teacher_Name)), stringsAsFactors = F)
teachers2watch$totalFs = 0
for(i in 1:nrow(teachers2watch)){
  id = teachers2watch$teacher[i]
  teachers2watch$totalFs[i] = sum(currentGrades.elaandmath.Fs$Teacher_Name == id, na.rm = T)
}
teachers2watch = teachers2watch[order(teachers2watch$teacher),]
rownames(teachers2watch) = NULL
write.csv(x = teachers2watch, file = paste0(OutFolder, "teachers.csv"))





