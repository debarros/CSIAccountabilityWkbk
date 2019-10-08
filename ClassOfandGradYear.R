# ClassOfandGradYear.R

# This looks at the current or most recent year and determines whether a student took classes during that year.


cc = cc.raw

# This only works if the grade levels in the workbook are accurate for the year we are looking at
# Determine which students have this as their 4th year of high school
# Determine whether they took classes at GTH this year 
# If they did take classes this year, determine what their grade level was this year
fourthYear = schoolYear(x = "full", y = BedsDate(2018))
for(i in 1:nrow(Workbook)){
  studID = Workbook$`Local.ID.(optional)`[i]
  if(Workbook$School.Year[i] == fourthYear){
    if(studID %in% cc$`[01]Student_Number`){
      Workbook$`Took.classes.at.GTH.that.year?`[i] = "Yes"
      Workbook$`Grade.level.during.that.year?`[i] = Workbook$`Grade.(leave.blank.if.no.longer.enrolled)`[i]
    } else {
      Workbook$`Took.classes.at.GTH.that.year?`[i] = "No"
    }
  }
}



# Determine which students graduated this calendar year
# If they did graduate this year, did they take classes at GTH this year?
# If they graduated this year and took classes this year, determine what their grade level was

gradYear = schoolYear(x = "year", y = BedsDate(2018)) + 1
for(i in 1:nrow(Workbook)){
  studID = Workbook$`Local.ID.(optional)`[i]                        # Get the ID
  grade = Workbook$`Grade.(leave.blank.if.no.longer.enrolled)`[i]   # Get the grade level
  if(!is.na(Workbook$Graduation.Year[i])){                          # If the student has a grad year
    if(Workbook$Graduation.Year[i] == gradYear){                    # And if it's the relevant grad year
      if(studID %in% cc$`[01]Student_Number`){                      # If the student took classes at GTH that year
        Workbook$`Took.classes.at.GTH.grad.year?`[i] = "Yes"        #   Mark took classes as yes
        Workbook$`Grade.level.during.grad.year?`[i] = grade         #   Copy over the grade level
      } else {                                                      # If the student did not take classes at GTH that year 
        Workbook$`Took.classes.at.GTH.grad.year?`[i] = "No"         #   Mark took classes as no and don't fill in the grade level
      }
    }
  }
}





write.csv(x = Workbook[,c("Local.ID.(optional)", "Took.classes.at.GTH.that.year?", "Grade.level.during.that.year?", "Took.classes.at.GTH.grad.year?", "Grade.level.during.grad.year?")], file = "fourth year and grad year info.csv")


