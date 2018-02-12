# Race and Ethnicity Breakdown.R

# This allows calculation of race and ethnicity info

#-----------------#
#### Load Data ####
#-----------------#

# generate the enrollment, student lite, and program service extracts from PowerSchool and load them here
EnrollExt = read.csv(file = file.choose(), header = F, stringsAsFactors = F)
StudLiteExt = read.csv(file = file.choose(), header = F, stringsAsFactors = F)

# Add column names to the SIRS extracts
colnames(StudLiteExt) = GetNiceColumnNames("STUDENT LITE", templates)[1:ncol(StudLiteExt)]
colnames(EnrollExt) = GetNiceColumnNames("SCHOOL ENTRY EXIT", templates)

# Determine the reporting ethnicity for each students
StudLiteExt$ReportEth = StudLiteExt$ETHNICCODESHORT      # Default to 1st race
StudLiteExt$ReportEth[!(StudLiteExt$RACE2 == "")] = "M"  # If more than 1 race, switch to multi
StudLiteExt$ReportEth[StudLiteExt$HISPANIC == "Y"] = "H" # If hispanic, switch to hispanic


x = EnrollExt$SCHOOLEXITDATE == ""                             # which exit dates are missing?
EnrollExt$SCHOOLEXITDATE[x] = as.character(schoolYear("end"))  # set missing exit dates to the end of the year
EnrollExt$SCHOOLEXITDATE = as.Date(EnrollExt$SCHOOLEXITDATE)   # convert exit date to date type
EnrollExt$SCHOOLENTRYDATE = as.Date(EnrollExt$SCHOOLENTRYDATE) # convert entry date to date type

y = EnrollExt$SCHOOLENTRYDATE < BedsDate()                # which students were around before BEDS day?
y = y & EnrollExt$SCHOOLEXITDATE > BedsDate()             # which students were around after BEDS day?
BEDSstudents = data.frame(ID = EnrollExt$STUDENTID[y])    # Get ID's of students enrolled on BEDS day
BEDSstudents$ReportEth = StudLiteExt$ReportEth[match(BEDSstudents$ID, StudLiteExt$STUDENTID)]
summary(as.factor(BEDSstudents$ReportEth))





output = data.frame(V1 = c("Asian:                               ",
                           "Native Hawaiian or Pacific Islander: ",
                           "Native American or Alaskan Native:   ",
                           "Black or African American:           ",
                           "White or Caucasian:                  ",
                           "Hispanic or Latino:                  ",
                           "Multiracial:                         ",
                           "Total:                               "), 
                    stringsAsFactors = F)

output = matrix(c("A", "Asian:                               ",
         "P", "Native Hawaiian or Pacific Islander: ",
         "N", "Native American or Alaskan Native:   ",
         "B", "Black or African American:           ",
         "W", "White or Caucasian:                  ",
         "H", "Hispanic or Latino:                  ",
         "M", "Multiracial:                         ",
         "T", "Total:                               "), 
       ncol = 2, byrow = T)
output = as.data.frame(output, stringsAsFactors = F)
colnames(output) = c("code", "category")

output$count = NA_character_
for(i in 1:(nrow(output) - 1)){
  output$count[i] = sum(BEDSstudents$ReportEth == output$code[i])
}
output$count[nrow(output)] = nrow(BEDSstudents)

maxLength = max(nchar(output$count), na.rm = T)

output$count = str_pad(string = output$count, width = maxLength, side = "left")

output = apply(output[c("category", "count")], 1, paste0, collapse = "")

output = c(paste0("Green Tech Reporting Ethnicity Counts as of BEDS Day, ", schoolYear(), ":") ,output)

write(output, paste0(OutFolder, "ethnic breakdown.txt"))  

