# ASAP to Access.R

# This script is for pulling regents data from ASAP and putting it into the regents database
# Ideally, it would involve one step: ASAP -> Access
# For the time being, it works more like this:
#    1. ASAP -> downloaded csv
#    2. downloaded csv -> regents.xlsx
#    3. regents.xslx -> combined csv
#    4. combined csv -> regents database
# This script currently only covers step 3.  Steps 1, 2, and 4 must be done manually.

# Until step 4 is implemented, the output should be a CSV with the following columns:
# ID, Exam, Score, Grade, Session, Location, InfoSource
#    Exam should match the values in the Exams table of the database
#    Score should be on a scale of 0 to 100 with no decimal point
#    Grade should be the student's grade level (studentInfo$grade_level)
#    Session should be YYYY Month (e.g. "2018 January")
#    Location should be "GTH"
#    InfoSource should be "ASAP"




#---------------------------------#
#### General Purpose Variables ####
#---------------------------------#

currentSession = "2018-01"
studentInfo = powerschoolraw




#-------------------------#
#### From regents.xlsx ####
#-------------------------#

# Extract data from regents.xslx workbooks and generate stuff to paste into the Regents database.
files = list.files(path = paste0("//stuthin2/Data/regents score exports/",currentSession), full.names = T, recursive = T)
files = grep(pattern = "regents.xlsx", x = files, ignore.case = T, value = T)
files = files[grep(pattern = "~$", x = files, fixed = T, invert = T)]


results = vector(mode = "list", length = length(files))
for(i in 1:length(files)){
  filename = files[i]
  results[[i]] = read.xlsx(xlsxFile = filename, sheet = "Scores for pasting")
}

results = rbindlist(results)
results$Score = as.integer(100 * results$Scaled.score.as.percentage)
results$TestName2 = substr(results$Test.Name, 1, nchar(results$Test.Name) - 6)
results$session = paste0(substr(currentSession, 1, 4), " ", month.name[as.integer(substr(currentSession, 6, 7))])
results$location = "GTH"
results$InfoSource = "ASAP"
results$Grade = studentInfo$grade_level[match(results$Student.ID, studentInfo$student_number)]
results$Exam = testLookup$Database[match(results$TestName2, testLookup$ASAP)]
results = results[,c("Student.ID", "Exam", "Score", "Grade", "session", "location", "InfoSource")]

write.csv(x = results, file = paste0(OutFolder, "paste into regents database.csv"))


#-------------------------#
#### From ASAP Exports ####
#-------------------------#

# Extract data from ASAP exports and generate stuff to paste into the Regents database.

files = list.files(path = paste0("//stuthin2/Data/regents score exports/",currentSession), full.names = T, recursive = T)
files = grep(pattern = "ASAPStudentItemAnalysisMulti", x = files, ignore.case = T, value = T)

allScores = vector(mode = "list", length = length(files))

filename = files[1]

for(filename in files){
  results = read.csv(file = filename, skip = 3, stringsAsFactors = F)
}
