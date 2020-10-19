# SummerSchoolProgramServices.R

# PowerSchool help on this process:
# https://docs.powerschool.com/USANY/state-reporting-setup/ny-program-services-import

# This requires an import file.  It should have a column with student IDs and a column with Program Service codes.
# The import file with student IDs and what course the student took can be obtained from the summer school coordinator.
# The program service codes can be found in the SIRS Manual in the Summer School Participation section.
# This was on page 237 in version 16.0.

ssps.in = read.xlsx(xlsxFile = file.choose())
str(ssps.in)

ssps.out = data.frame(Student_Number = ssps.in$Student.Number,            # Make sure the ssps.in field name matches the import file
                      ProgramServiceCode = ssps.in$Program.Service.Code)  # Make sure the ssps.in field name matches the import file
ssps.out$ProgramServiceCategory = "OTHER"
ssps.out$BeginningDate = "07/06/2020"                                     # first day of summer school
ssps.out$EndingDate = "07/31/2020"                                        # last day of summer school
ssps.out$CTETechPrepCategory = ""
ssps.out$ProgramIntensity = ""
ssps.out$ExitReasonCode = ""
ssps.out$ParticipationInfoCode = ""
ssps.out$StateLocationID = ""                                             # It's possible that this should be something.  Not sure.
ssps.out$Created_By = "pdbarros"                                          # PowerSchool login name of person doing the upload
ssps.out$ProgramServiceLevel = "S"                                        # This should be S for School
ssps.out$SchoolID = "100860907"
ssps.out$PROGRAMELIGIBILITYCODE1 = ""
ssps.out$PROGRAMELIGIBILITYCODE2 = ""
ssps.out$PROGRAMELIGIBILITYCODE3 = ""
ssps.out$PROGRAMELIGIBILITYCODE4 = ""
ssps.out$PROGRAMELIGIBILITYCODE5 = ""
ssps.out$NIGHTTIMERESIDENCE = ""


print("see the SummerProgramServiceImport.csv file.  It will need to be imported into PowerSchool.")
print("Special Functions > importing and Exporting > NY Student Program Service Import")
write.table(ssps.out, paste0(OutFolder,"SummerProgramServiceImport.csv"), quote = F, row.names = F, col.names = T, sep = "," , dec = ".")
