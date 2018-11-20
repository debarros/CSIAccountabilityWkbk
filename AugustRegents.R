# AugustRegents.R
# This takes csv exports from ASAP and creates an ASSESSMENT FACT export to be uploaded to Level 0

# Read the column names from the eScholar template file and remove special characters from them
AssessmentColumns = GetNiceColumnNames("ASSESSMENT FACT", templates)

# Export csv's of the Rank List by Building from ASAP and put them (and nothing else) in a folder
# The following section reads in the files and them combines them into a single data.frame
filelist = list.files(path = "\\\\stuthin2/data/2018-2019/Level 0/Assessment Fact/exports", full.names = T)
scores = vector(mode = "list", length = length(filelist))
for(i in 1:length(filelist)){
  x = read.csv(filelist[i], stringsAsFactors = F)
  scores[[i]] = x
}
scores = rbindlist(scores)


# Set up the output
output = matrix(data = NA_character_, nrow = nrow(scores), ncol = length(AssessmentColumns))
output = as.data.frame(output, stringsAsFactors = F)
colnames(output) = AssessmentColumns

# Load the static data into the output
output$DISTRICTCODEDISTRICTOFRESPONSIBILITYCODE = "80059776"
output$TESTDESCRIPTIONTESTGROUP = "Regents"
output$ASSESSMENTSCHOOLYEARDATEVERSION = schoolYear(x = "end") # June 30th of the school year
output$TESTINGLOCATIONCODE = "1"
output$ASSESSMENTLANGUAGECODE = "ENG"


# Load the specific data into the output
output$SUBTESTIDENTIFIERASSESSMENTMEASURESTANDARDDESCRIPTION = substr(x = scores$textbox14, start = 11, stop = nchar(scores$textbox14)) 
output$TESTDATEASSESSMENTDATEOFADMINISTRATION = substr(x = scores$textbox17, start = 12, stop = nchar(scores$textbox17))
output$STUDENTIDSCHOOLDISTRICTSTUDENTID = scores$StudentID_1
output$NUMERICSCORE = scores$ScaledScore_1

i = 1
standardCodes = read.xlsx("standard achieved codes.xlsx", sheet = "Standard")
standardCodeNames = read.xlsx("standard achieved codes.xlsx", sheet = "Standard", rows = 1, rowNames = F, colNames = F)
colnames(standardCodes) = standardCodeNames
alternateCodes = read.xlsx("standard achieved codes.xlsx", sheet = "Alternate")
alternateCodeNames = read.xlsx("standard achieved codes.xlsx", sheet = "Alternate", rows = 1, rowNames = F, colNames = F)
colnames(alternateCodes) = alternateCodeNames

for(i in 1:nrow(output)){
  currentTest = output$SUBTESTIDENTIFIERASSESSMENTMEASURESTANDARDDESCRIPTION[i]
  currentScore = output$NUMERICSCORE[i]
  
  #standard achieved 
  output$STANDARDACHIEVEDCODEASSESSMENTSTANDARDMETCODE[i] = standardCodes[standardCodes$Score == currentScore, currentTest]
  
  # Alternate standard achieved
  if(currentTest %in% alternateCodeNames){
    output$ALTERNATESTANDARDACHIEVEDCODE[i] = alternateCodes[alternateCodes$Score == currentScore, currentTest]  
  }
  
}



output = DFna.to.empty(output)


write.table(output, file = paste0(OutFolder, "AugustRegents.csv"), row.names = F, col.names = F, sep = ",", dec = ".") 
