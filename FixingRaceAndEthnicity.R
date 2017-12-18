# WorkbookEthnicity.R


# This script uses the Student Lite extract, the PowerSchool Students table, and the Accountability Workbook.
# It tries to find errors and missing information in each location and provide correct info where possible.
# This does not look for students who are missing their race codes in PowerSchool.  Use the Level0 script for that.

#-------------------------------#
#### PowerSchool Ethnicities ####
#-------------------------------#

# First, find bad ethnicities in PowerSchool
powerschool = powerschoolraw
goodracecodes = c("B","W","A","P","I","H","M")
fixPSrace = powerschool$student_number[!(powerschool$Ethnicity %in% goodracecodes) & !is.na(powerschool$Ethnicity)]
if(length(fixPSrace) > 0){
  print(paste0(fixPSrace, collapse = ","))
  print("Find these students in PowerSchool and fix their Scheduling/Reporting Ethnicity on the demographics page.")
  print("Then reload info and continue.")
} else {
  print("No students have crazy race/ethnicity selections in PowerSchool.  Continue with this script.")
}

# Next, find innacurate ethnicities in this year's students

# load the demographics file produced by PowerSchool
demographics = read.csv(file.choose() ,header = F, stringsAsFactors = F)
demographics = DFna.to.empty(demographics)
# Use the nysed template to assign the column names to the demographics file
colnames(demographics) = dBtools::GetNiceColumnNames("STUDENT LITE", templates)
demographics = demographics[demographics$ETHNICCODESHORTRACE1CODE != "",]
demographics$PSRace = powerschool$Ethnicity[match(demographics$STUDENTIDSCHOOLDISTRICTSTUDENTID, powerschool$student_number)]
demographics$PSRace = na.to.empty(demographics$PSRace)
demographics$AccurateRace = demographics$ETHNICCODESHORTRACE1CODE
for(i in 1:nrow(demographics)){
  if(demographics$HISPANICETHNICITYINDICATORHISPANICLATINOETHNICITYINDICATOR[i] == "Y"){
    demographics$AccurateRace[i] = "H"
  } else if(demographics$RACE2CODE[i] != ""){
    demographics$AccurateRace[i] = "M"
  }
}

demoVars = c("STUDENTIDSCHOOLDISTRICTSTUDENTID",	"LASTNAMESHORTSTUDENTSLASTNAME", "FIRSTNAMESHORTSTUDENTSFIRSTNAME", 
             "ETHNICCODESHORTRACE1CODE", "HISPANICETHNICITYINDICATORHISPANICLATINOETHNICITYINDICATOR", "RACE2CODE", 
             "RACE3CODE", "RACE4CODE", "RACE5CODE", "PSRace", "AccurateRace")


innacEthnic = demographics$STUDENTIDSCHOOLDISTRICTSTUDENTID[!(VbetterComp(demographics$AccurateRace, demographics$PSRace))]
if(length(innacEthnic) > 0){
  print("There are students with innacurate Scheduling/Reporting Ethnicity on the demographics page.  See the csv.")
  print("Fix all that stuff then reload the data and continue.")
  write.csv(demographics[demographics$STUDENTIDSCHOOLDISTRICTSTUDENTID %in% innacEthnic, demoVars], "innacuratePSethnicities.csv")
} else {
  print("There are no innacurate Scheduling/Reporting Ethnicities in PowerSchool. Continue with this script.")
}


Workbook$PSethnicity = powerschool$Ethnicity[match(Workbook$`Local.ID.(optional)`, powerschool$student_number)]
unique(Workbook$PSethnicity)


fixedRaces = data.frame(Old = c("B", "H", "A", "O", "M", "W", "I", "P"), 
                        New = c("Black or African American", "Hispanic or Latino", "Asian or Native Hawaiian/Other Pacific Islander",
                                "Problem with PowerSchool ethnicity", "Multiracial", "White", "American Indian or Alaskan Native",
                                "Asian or Native Hawaiian/Other Pacific Islander"), 
                        stringsAsFactors = F)

Workbook$Ethnicity2 = Workbook$Ethnicity
for(i in 1:nrow(Workbook)){
  if(is.na(Workbook$Ethnicity2[i]) & !is.na(Workbook$PSethnicity[i]) & Workbook$`Still.Enrolled?`[i] == "yes"){
    newEthnicity = fixedRaces$New[match(Workbook$PSethnicity[i], fixedRaces$Old)]
    Workbook$Ethnicity2[i] = newEthnicity
  }
}

# These lines allow fixing weird typos
# unique(Workbook$Ethnicity2)
# Workbook$Ethnicity2[Workbook$Ethnicity2 == "American Indian or Alaska Native"] = "American Indian or Alaskan Native"
# Workbook$Ethnicity2[Workbook$Ethnicity2 == "Asian"] = "Asian or Native Hawaiian/Other Pacific Islander"
# Workbook$Ethnicity2[Workbook$Ethnicity2 == "White "] = "White"

if("Problem with PowerSchool ethnicity" %in% Workbook$Ethnicity2){
  print("There are some problematic ethnicities in PowerSchool.  Check the view.")
  View(Workbook[VbetterComp(Workbook$Ethnicity2, "Problem with PowerSchool ethnicity"),])
}

Workbook$Ethnicity2[VbetterComp(Workbook$Ethnicity2, "Problem with PowerSchool ethnicity")] = ""
Workbook$Ethnicity2 = na.to.empty(Workbook$Ethnicity2)

if(!(all(VbetterComp(Workbook$Ethnicity2, Workbook$Ethnicity)))){
  print("The following students need their ethnicities fixed in the workbook")
  print("If there are a lot of them, use the CSV file.")
  print(Workbook[(!(VbetterComp(Workbook$Ethnicity2, Workbook$Ethnicity))),c(2:4,9,14,178)])
  write.csv(x = Workbook[,c("Local.ID.(optional)", "First.Name", "Last.Name", "Cohort.Year.(year.1st.entered.9th)", "Ethnicity2")], 
            file = "fixedWorkbookEthnicities.csv")
}


