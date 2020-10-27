#crdc.R

# This prepares data for the CRDC (Civil Rights Data Collection)

BEDSday = BedsDate(year = 2017)
EndDay = schoolYear(x = "end", y = BEDSday) - 29

# Read in the last Student Lite file from the relevant year
StudentLiteExtract = read.csv(file = file.choose(), header = F, stringsAsFactors = F)
colnames(StudentLiteExtract) = GetNiceColumnNames("STUDENT LITE", templates)[1:ncol(StudentLiteExtract)]


# Read in the last School Enrollment file from the relevant year
EnrollExt = read.csv(file = file.choose(), header = F, stringsAsFactors = F)
colnames(EnrollExt) = GetNiceColumnNames("SCHOOL ENTRY EXIT", templates)
EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE[EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE == ""] = as.character(schoolYear(x = "end", y = BEDSday))


# Calculate the reporting race
StudentLiteExtract$CRDC_Race = NA
StudentLiteExtract$CRDC_Race = StudentLiteExtract$ETHNICCODESHORTRACE1CODE
StudentLiteExtract$CRDC_Race[StudentLiteExtract$RACE2CODE != ""] = "M"
StudentLiteExtract$CRDC_Race[StudentLiteExtract$HISPANICETHNICITYINDICATORHISPANICLATINOETHNICITYINDICATOR == "Y"] = "H"
summary(factor(StudentLiteExtract$CRDC_Race))


# Add the PowerSchool ID
StudentLiteExtract$ID = powerschoolraw$ID[match(x = StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID, table = powerschoolraw$student_number)]


# In the enrollment file, mark whether the enrollment covered BEDS day
EnrollExt$IncludesBEDS = T
EnrollExt$IncludesBEDS[EnrollExt$SCHOOLENTRYDATEENROLLMENTENTRYDATE > BEDSday] = F
EnrollExt$IncludesBEDS[EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE < BEDSday] = F


# In the demographics file, mark whether the student was enrolled on beds day
StudentLiteExtract$EnrolledOnBEDS = F
for(i in 1:nrow(StudentLiteExtract)){
  thisOne = sum(EnrollExt$IncludesBEDS & EnrollExt$STUDENTIDSCHOOLDISTRICTSTUDENTID == StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID[i])
  StudentLiteExtract$EnrolledOnBEDS[i] = thisOne > 0
}



# In the demographics file, add info about IDEA, 504, and ELL
StudentLiteExtract$IDEA = Workbook$IEP[match(StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID, Workbook$`Local.ID.(optional)`)]
StudentLiteExtract[is.na(StudentLiteExtract$IDEA),4:8]

StudentLiteExtract$Five04 = Workbook$`504.plan?`[match(StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID, Workbook$`Local.ID.(optional)`)]
StudentLiteExtract[is.na(StudentLiteExtract$Five04),4:8]

StudentLiteExtract$ELL = Workbook$LEP.Status[match(StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID, Workbook$`Local.ID.(optional)`)]
StudentLiteExtract[is.na(StudentLiteExtract$ELL),4:8]



# Get an enrollment file from the following year
NextLite = read.csv(file = file.choose(), header = F, stringsAsFactors = F)
colnames(NextLite) = GetNiceColumnNames("STUDENT LITE", templates)[1:ncol(NextLite)]


# Add in subsequent grade levels to StudentLiteExtract
StudentLiteExtract$NextGrade = NextLite$CURRENTGRADELEVELGRADELEVEL[match(StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID, NextLite$STUDENTIDSCHOOLDISTRICTSTUDENTID)]


# Mark whether students were retained
StudentLiteExtract$Retained = StudentLiteExtract$CURRENTGRADELEVELGRADELEVEL == StudentLiteExtract$NextGrade
StudentLiteExtract$Retained[is.na(StudentLiteExtract$Retained)] = F


# Mark whether the students were still enrolled as of the abritrary end of year date
StudentLiteExtract$EndActive = T
for(i in 1:nrow(StudentLiteExtract)){
  curID = StudentLiteExtract$STUDENTIDSCHOOLDISTRICTSTUDENTID[i]
  endDates = EnrollExt$SCHOOLEXITDATEENROLLMENTEXITDATE[EnrollExt$STUDENTIDSCHOOLDISTRICTSTUDENTID == curID]
  endDates = betterMax(endDates)
  StudentLiteExtract$EndActive[i] = endDates > EndDay
}

outfile = StudentLiteExtract[,c("STUDENTIDSCHOOLDISTRICTSTUDENTID", "LASTNAMESHORTSTUDENTSLASTNAME",	"FIRSTNAMESHORTSTUDENTSFIRSTNAME",
                                "CURRENTGRADELEVELGRADELEVEL", "ID","GENDERCODEGENDERDESCRIPTION", "CRDC_Race", "IDEA", "Five04",
                                "ELL", "Retained", "EnrolledOnBEDS", "EndActive")]

write.csv(x = outfile, file = paste0(OutFolder, "crdc student table.csv"))

