# LoginMailings.R

# This was created to help generate mailings (email and print) for distributing google classroom links to students

# Establish the data
students = powerschoolraw
cc = cc.raw
emails = read.csv(file.choose())
sect = sections
gcLinks = read.xlsx(xlsxFile = PSLocation, sheet = "GC Links")


# Isolate the teacher's last name
sect$LN = ""
for(i in 1:nrow(sect)){
  stopChar = regexpr(pattern = ",",text = sect$`[05]lastfirst`[i]) - 1
  sect$LN[i] = substr(x = sect$`[05]lastfirst`[i], start = 1, stop = stopChar)
}


# Remove useless sections
sect = sect[sect$TermID >= 3000,]
sect = sect[!is.na(sect$External_Expression),]
rownames(sect) = NULL

# Determine the period for each section
sect$period = ""
sect$period = substr(x = sect$External_Expression, start = 1, stop = 1)
sect$period[sect$External_Expression == "11(A)"] = "L"
sect$period[sect$External_Expression == "HR(A)"] = "A"
sect$period[sect$External_Expression == "HRA(A)"] = "A"
for(i in 1:nrow(sect)){
  if(sect$period[i] == "P"){
    sect$period[i] = substr(x = sect$External_Expression[i], start = 2, stop = 2)
  }
}
 


# Create the teacher-period field
sect$TeachPer = paste0(sect$LN, " - ", sect$period)


# Put the gcLinks in the section table
sect$gcLink = ""
sect$gcLink = gcLinks$Google.Class.Room.Link[match(sect$TeachPer, gcLinks$TeachPer)]
sect$Code = ""
sect$Code = gcLinks$Code[match(sect$TeachPer, gcLinks$TeachPer)]
sect$Handle = ""
sect$Handle = gcLinks$Contact.Email[match(sect$TeachPer, gcLinks$TeachPer)]
sect$Handle = sub(pattern = "@greentechhigh.org", replacement = "", x = sect$Handle, fixed = T)


# Remove unnecessary enrollments
cc = cc[cc$SectionID > 0,]       # eliminate dropped enrollments
cc = cc[cc$TermID < 3002,]       # eliminate spring courses


# Add the gc link and period number from the sections table to the cc table
cc$gcLink = ""
cc$gcLink = sect$gcLink[match(cc$SectionID, sect$ID)]
cc$period = ""
cc$period = sect$period[match(cc$SectionID, sect$ID)]
cc$Code = sect$Code[match(cc$SectionID, sect$ID)] 
cc$Handle = sect$Handle[match(cc$SectionID, sect$ID)]
cc$gradelevel = currentStudents$grade_level[match(cc$`[01]Student_Number`, currentStudents$student_number)]
# cc = cc[cc$gradelevel > 7,]
# currentStudents = currentStudents[currentStudents$grade_level > 7, ]

# Remove duplicate enrollments
cc$SuperCode = paste0(cc$Course_Number, cc$section_number, cc$TermID, cc$Teacher, cc$`[01]Student_Number`, cc$period)
cc = cc[!duplicated(cc$SuperCode),]


# Reorganize the cc table so it's wide, with one row per student.  Maybe base this on the current students table?
periods = c("A",1:8)
colnames(cc)[colnames(cc) == "[05]lastfirst"] = "Teacher"
colnames(cc)[colnames(cc) == "[02]course_name"] = "Course"
fields = c("Teacher", "Course", "gcLink", "Code", "Handle")
columns2add = CharCartProd(x = fields, y = periods)

currentStudents[,columns2add] = ""
thisPeriod = periods[1]
thisField = fields[1]
thisStudRow = 1
for(thisPeriod in periods){                                                 # For each period
  print(paste0("thisPeriod =", thisPeriod))
  for(thisField in fields){                                                 # For each field (teacher, course, and link)
    # print(paste0("thisField =", thisField))
    for(thisStudRow in 1:nrow(currentStudents)){                            # For each student
      # print(paste0("thisStudRow =", thisStudRow))
      thisID = currentStudents$student_number[thisStudRow]                  # Get the student ID
      thisStudCC = cc[cc$`[01]Student_Number` == thisID,]                   # Get the subset of cc that has that student's enrollments
      if(nrow(thisStudCC) > 0){                                             # If the student has any,
        thisRowCC = thisStudCC[thisStudCC$period == thisPeriod,]            # Get the enrollment for the current period
        if(nrow(thisRowCC) > 0){                                            # If it exists,
          thisColumn = paste0(thisField,thisPeriod)                         # Set the column of currentStudents where the data will go
          currentStudents[thisStudRow, thisColumn] = thisRowCC[,thisField]  # Put the relevant data in the relevant cell
        } # \if the student has an enrollment for this period
      } #  \if the student has any enrollments
    } # \for each student
  } # \for each field
} # \for each period






# put in the email addresses from the file that Aviza sent into the new wide cc/student table
currentStudents$email1 = ""
currentStudents$email2 = ""
currentStudents$email3 = ""

currentStudents$email1 = emails$Parent.Email.1..mom.[match(currentStudents$student_number, emails$Student.ID)]
currentStudents$email2 = emails$Parent.Email.2..Dad..[match(currentStudents$student_number, emails$Student.ID)]
currentStudents$email3 = emails$Parent.Email.3..Guardian.[match(currentStudents$student_number, emails$Student.ID)]





# Export

currentStudents = DFna.to.empty(currentStudents)
write.csv(x = currentStudents, file = paste0(OutFolder, "mailing merge stuff.csv"))
