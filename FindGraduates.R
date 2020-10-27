# FindGraduates.R

# This file is to help with the submission process, where you send a file to CSI in September.
# In the summer, graduates must be added to workbook.
# This file reads in the most recent enrollment and demographics exports from PowerSchool.

# When loading these files, navigate to the folders for level 0 uploads and choose 
# the most recent version of each export
studentLite = read.csv(file.choose(), stringsAsFactors = F, header = F)
enrollment = read.csv(file.choose(), stringsAsFactors = F, header = F)

# Get the header rows
a = read.xlsx(xlsxFile = "J:/SIRS manuals templates guides etc/2016-17eScholarTemplatesNYS20161209.xlsx", 
              sheet = "STUDENT LITE", 
              startRow = 4,
              cols = 3,
              colNames = F) 
b = read.xlsx(xlsxFile = "J:/SIRS manuals templates guides etc/2016-17eScholarTemplatesNYS20161209.xlsx", 
              sheet = "SCHOOL ENTRY EXIT", 
              startRow = 4,
              cols = 3,
              colNames = F) 

a = a[,1]
b = b[,1]
a = gsub(pattern = " +", "_", a)
a = gsub(pattern = "\\W", "", a)
b = gsub(pattern = " +", "_", b)
b = gsub(pattern = "\\W", "", b)


# Apply the header rows
colnames(studentLite) = a
colnames(enrollment) = b


#Fix date variables
demoDateVars = c("SCHOOL_YEAR_DATE", "BIRTH_DATEDATE_OF_BIRTH", "LAST_STATUS_DATE", "GRADE_09_ENTRY_DATEFIRST_DATE_OF_ENTRY_INTO_GRADE_9", "INOCULATION_DATEIMMUNIZATION_DATE_FOR_FIRST_POLIO_VACCINATION", "INITIAL_US_ENTRY_DATEDATE_OF_ENTRY_INTO_UNITED_STATES")
enrollDateVars = c("SCHOOL_YEAR_DATE", "SCHOOL_ENTRY_DATEENROLLMENT_ENTRY_DATE", "SCHOOL_EXIT_DATE_ENROLLMENT_EXIT_DATE")

for(i in demoDateVars){
  studentLite[,i] = as.Date(studentLite[,i])
}

enrollment$SCHOOL_EXIT_DATE_ENROLLMENT_EXIT_DATE[enrollment$SCHOOL_EXIT_DATE_ENROLLMENT_EXIT_DATE == ""] = NA
for(i in enrollDateVars){
  enrollment[,i] = as.Date(enrollment[,i])
}

# Limit enrollment records to only the most recent for each student
enrollment = enrollment[order(enrollment$SCHOOL_ENTRY_DATEENROLLMENT_ENTRY_DATE, decreasing = T),]
rownames(enrollment) = NULL
enrollment = enrollment[!duplicated(enrollment$STUDENT_ID_SCHOOL_DISTRICT_STUDENT_ID),]
enrollment = enrollment[order(enrollment$STUDENT_ID_SCHOOL_DISTRICT_STUDENT_ID),]
studentLite = studentLite[order(studentLite$STUDENT_IDSCHOOL_DISTRICT_STUDENT_ID),]

#The next line shoudl return 0.  If not, you are in trouble because the sets of student IDs don't match
sum(!(enrollment$STUDENT_ID_SCHOOL_DISTRICT_STUDENT_ID %in% studentLite$STUDENT_IDSCHOOL_DISTRICT_STUDENT_ID))

# enrollment[!(enrollment$STUDENT_ID_SCHOOL_DISTRICT_STUDENT_ID %in% studentLite$STUDENT_IDSCHOOL_DISTRICT_STUDENT_ID),]
# studentLite[!(studentLite$STUDENT_ID_SCHOOL_DISTRICT_STUDENT_ID %in% enrollment$STUDENT_IDSCHOOL_DISTRICT_STUDENT_ID),]
# enrollIDs = enrollment$STUDENT_ID_SCHOOL_DISTRICT_STUDENT_ID
# demoIDs = studentLite$STUDENT_IDSCHOOL_DISTRICT_STUDENT_ID
# intersect(enrollIDs, demoIDs)
# setdiff(enrollIDs, demoIDs)
# setdiff(demoIDs, enrollIDs)

# The next line should return TRUE.  If not, the ID's are not in the same order.
all(VbetterComp(enrollment$STUDENT_ID_SCHOOL_DISTRICT_STUDENT_ID, studentLite$STUDENT_IDSCHOOL_DISTRICT_STUDENT_ID))

studentLite$exitdate = enrollment$SCHOOL_EXIT_DATE_ENROLLMENT_EXIT_DATE
studentLite$exitreason = enrollment$SCHOOL_EXIT_TYPE_CODE_REASON_FOR_ENDING_ENROLLMENT_CODE


unique(studentLite$exitreason)

graduates = studentLite[!is.na(studentLite$DIPLOMA_TYPE_CODE_CREDENTIAL_TYPE_CODE),]

graduates$cohort = substr(graduates$GRADE_09_ENTRY_DATEFIRST_DATE_OF_ENTRY_INTO_GRADE_9,1,4)

advancedCodes = c(680, 204, 221, 238, 697, 255, 272, 289, 714, 306, 323, 340, 731, 357, 374, 391)
regentsCodes = c(779, 796, 762, 813)
localCodes = c(068, 612)


graduates$DiplomaType[graduates$DIPLOMA_TYPE_CODE_CREDENTIAL_TYPE_CODE %in% advancedCodes] = "Advanced"
graduates$DiplomaType[graduates$DIPLOMA_TYPE_CODE_CREDENTIAL_TYPE_CODE %in% regentsCodes] = "Regents"
graduates$DiplomaType[graduates$DIPLOMA_TYPE_CODE_CREDENTIAL_TYPE_CODE %in% localCodes] = "Local"

graduates = graduates[order(graduates$cohort, graduates$exitdate, graduates$LAST_NAME_SHORT_STUDENTS_LAST_NAME, graduates$FIRST_NAME_SHORT_STUDENTS_FIRST_NAME),]

View(graduates[,c("cohort", "STUDENT_IDSCHOOL_DISTRICT_STUDENT_ID", "LAST_NAME_SHORT_STUDENTS_LAST_NAME", "FIRST_NAME_SHORT_STUDENTS_FIRST_NAME", "exitdate", "exitreason", "DiplomaType")])


View(graduates[graduates$cohort == 2011, c("cohort", "STUDENT_IDSCHOOL_DISTRICT_STUDENT_ID", "LAST_NAME_SHORT_STUDENTS_LAST_NAME", "FIRST_NAME_SHORT_STUDENTS_FIRST_NAME", "exitdate", "exitreason", "DiplomaType")])


x = graduates[!is.na(graduates$exitdate),]
sum(x$exitdate > as.Date("2016-08-31"))
nrow(graduates)
graduates$exitdate[is.na(graduates$exitdate)] = as.Date("2017-08-15")
sum(graduates$exitdate > as.Date("2016-08-31"))

table(graduates$cohort)
