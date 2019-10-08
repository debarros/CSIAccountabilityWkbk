# Credits.R

# The purpose of this script is to calculate the number of credits each student has earned
# This is done based on F2 grades.
# If credits are not stored properly, this will not calculate credits accurately.
# Before this is run, the F1F2 script should be run to identify problems in stored grades

# Run the MainScript first

Workbook$pasteCredits = 0
Workbook$pasteGradeLevels = ""

for(i in 1:nrow(Workbook)){                                                           # For each student in the workbook,
  currentID = Workbook$`Local.ID.(optional)`[i]                                       # Grab the ID
  credits = sum(F2$EarnedCrHrs[F2$`[1]Student_Number` == currentID])                  # Calculate credits in PowerSchool
  Workbook$pasteCredits[i] = betterMax(c(credits, Workbook$Total.Credits.Earned[i]))  # Take the max of credits (wkbk vs PS)
}

write.csv(x = Workbook[c("Local.ID.(optional)", "pasteCredits")], file = paste0(OutFolder,"credits for pasting.csv"))


# for(i in 2011:2017){
#   writeableset = Workbook[Workbook$`Cohort.Year.(year.1st.entered.9th)` == i,c("Local.ID.(optional)", "Last.Name", "First.Name", "pasteGradeLevels", "pasteCredits")]
#   if(99 %in% writeableset$pasteGradeLevels){
#     stop("ERROR!  A student is showing up with a grade level of 99.")
#   }
#   write.csv(x = writeableset, file = paste0(OutFolder,i,"_credits.csv"))
# }


