# ClassOfandGradYear.R

# This looks at the current or most recent year and determines whether a student took classes during that year.
# This will not blank out any info but it might enter the wrong info if it it run improperly

# Note: cc must be from the relevant year
cc = cc_prior.raw

# fourthYear must be for the same year as cc_prior.raw
fourthYear = schoolYear(x = "full", y = BedsDate() - 665)
gradYear = as.integer(substr(fourthYear, 6, 9))

print(cc$DateLeft[1])
print(fourthYear)
print(gradYear)

# This only works if the grade levels in the workbook are accurate for the year we are looking at
# Determine which students have this as their 4th year of high school
# Determine whether they took classes at GTH this year 
# If they did take classes this year, determine what their grade level was this year
# Students not in their 4th year for the year we are examining are skipped.
for(i in 1:nrow(Workbook)){                      # For each student
  studID = Workbook$`Local.ID.(optional)`[i]     # get the student ID
  if(Workbook$School.Year[i] == fourthYear){     # If that student's 4th year in high school is the one we're working on right now,
    if(studID %in% cc$`[01]Student_Number`){     # If that student appears in the cc table,
      Workbook$`Took.classes.at.GTH.that.year?`[i] = "Yes"   # Mark classes as "yes"
      # grade = Workbook$`Grade.(leave.blank.if.no.longer.enrolled)`[i]   # Copy the current grade level
      # Workbook$`Grade.level.during.that.year?`[i] = grade
    } else {
      Workbook$`Took.classes.at.GTH.that.year?`[i] = "No"
    }
  }
}



# Determine which students graduated this calendar year
# If they did graduate this year, did they take classes at GTH this year?
# If they graduated this year and took classes this year, determine what their grade level was
# Students who did not graduate in the year we are examining are skipped

for(i in 1:nrow(Workbook)){
  studID = Workbook$`Local.ID.(optional)`[i]                        # Get the ID
  grade = Workbook$`Grade.(leave.blank.if.no.longer.enrolled)`[i]   # Get the grade level
  if(!is.na(Workbook$Graduation.Year[i])){                          # If the student has a grad year
    if(Workbook$Graduation.Year[i] == gradYear){                    # And if it's the relevant grad year
      if(studID %in% cc$`[01]Student_Number`){                      # If the student took classes at GTH that year
        Workbook$`Took.classes.at.GTH.grad.year?`[i] = "Yes"        #   Mark took classes as yes
        # Workbook$`Grade.level.during.grad.year?`[i] = grade         #   Copy over the grade level
      } else {                                                      # If the student did not take classes at GTH that year 
        Workbook$`Took.classes.at.GTH.grad.year?`[i] = "No"         #   Mark took classes as no and don't fill in the grade level
      }
    }
  }
}



wkbkOutput = Workbook[,c("Local.ID.(optional)", "Took.classes.at.GTH.that.year?", "Grade.level.during.that.year?", "Took.classes.at.GTH.grad.year?", "Grade.level.during.grad.year?")]

wkbkOutput = DFna.to.empty(wkbkOutput)

write.csv(x = wkbkOutput, file = paste0(OutFolder,"fourth year and grad year info.csv"))


