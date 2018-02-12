# ASAP to Access.R




#--------------------------#
#### Straight from ASAP ####
#--------------------------#

# Extract data from ASAP exports and generate stuff to paste into the Regents database.

currentSession = "2018-01"


files = list.files(path = paste0("//stuthin2/Data/regents score exports/",currentSession), full.names = T, recursive = T)
files = grep(pattern = "ASAPStudentItemAnalysisMulti", x = files, ignore.case = T, value = T)

allScores = vector(mode = "list", length = length(files))

filename = files[1]

for(filename in files){
  results = read.csv(file = filename, skip = 3, stringsAsFactors = F)
}





#-------------------------#
#### From regents.xlsx ####
#-------------------------#

# Extract data from regents.xslx workbooks and generate stuff to paste into the Regents database.
files = list.files(path = paste0("//stuthin2/Data/regents score exports/",currentSession), full.names = T, recursive = T)
files = grep(pattern = "regents.xlsx", x = files, ignore.case = T, value = T)
files = grep(pattern = "~$", x = files, ignore.case = T, value = T)

filename = files[2]
for(filename in files){
  results = read.xlsx(xlsxFile = filename, sheet = "Scores for pasting")
}