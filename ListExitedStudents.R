# ListExitedStudents.R
# This script finds all of the students who have recently exited GTH 

# Set a cutoff for what you consider to be a recent exit
Deadline = schoolYear(x = "end", y = schoolYear("end") - 400) - 15               # Defaults to June 15 of prior school year


# Narrow the data to the relevant students
exiters = Workbook[!(VbetterComp(Workbook$Discharge.Reason, "never attended")),] # remove the no-show students
exiters = exiters[exiters$`Still.Enrolled?` %in% c("No", "NO", "no"),]           # Subset to just existed students
exiters = exiters[exiters$Date.left.GTH >= Deadline,]                            # Subset to just students who left after the deadline
exiters = exiters[!(exiters$Discharge.Reason %in% c("graduated","Graduated")),]  # Remove students who graduated


# Format and create the output
exiters = exiters[,c("Local.ID.(optional)", "Last.Name", "First.Name", "Cohort.Year.(year.1st.entered.9th)", 
                     "Date.First.Enrolled.at.GTH", "Date.left.GTH", "Discharge.Reason", "Subsequent.Placement")]
write.csv(exiters, "RecentlyExitedStudents.csv", row.names = F)
