# BEDS day FRPL.R

# This is useful for completing the IMF
# It requires that all FRPL records (from both DCMP and lunch forms) be entered in PowerSchool already.

# Get the enrollment
enrollment = read.csv(file.choose(), header = F, stringsAsFactors = F)                                        # load a current school enrollment extract
colnames(enrollment) = GetNiceColumnNames("SCHOOL ENTRY EXIT", templates)                                     # set the column names
enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE[enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE == ""] = "2018-06-30" # set missing exit dates to the end of the year
enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE = as.Date(enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE)            # convert exit date to date type
enrollment$SCHOOLENTRYDATEENROLLMENTENTRYDATE = as.Date(enrollment$SCHOOLENTRYDATEENROLLMENTENTRYDATE)        # convert entry date to date type

BEDSstudents = enrollment$STUDENTIDSCHOOLDISTRICTSTUDENTID[                                          # Get a vector of ID's of students who were enrolled on BEDS day
  enrollment$SCHOOLENTRYDATEENROLLMENTENTRYDATE < as.Date("2017-10-04") & 
    enrollment$SCHOOLEXITDATEENROLLMENTEXITDATE > as.Date("2017-10-04")]

# Get the program facts
programservices = read.csv(file.choose(), stringsAsFactors = F, header = F)                          # load a current program services extract
programColumns = read.xlsx(templates, sheet = "PROGRAMS FACT", startRow = 2)[,3]                     # load the column names
for(i in 1:length(programColumns)){programColumns[i] = gsub("[^[:alnum:]]", "", programColumns[i])}  # clean the column names
colnames(programservices) = programColumns                                                           # set the column names
lunchservices = programservices[programservices$PROGRAMSCODEPROGRAMSERVICECODE %in% c(5806, 5817),]  # subset to just program services
bedsLunchServices = lunchservices[lunchservices$STUDENTIDSCHOOLDISTRICTSTUDENTID %in% BEDSstudents,] # subset to just students who were enrolled on BEDS day

# Summarize the results
summary(factor(bedsLunchServices$PROGRAMSCODEPROGRAMSERVICECODE))
