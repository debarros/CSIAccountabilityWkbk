# L2Regents.R

# This script compares scores from the accountability workbook to scores from the SIRS-202 Total Cohort Assessment Reports
# As always, run Mainscript first.
# It might also be a good idea to run the L2cohorts.R script first.

# Download the detail report (Excel 2007 Data) of the SIRS-202 for each of the last 6 cohorts
# Compile them into one sheet in a file called Total Cohort - Assessment Detail.xlsx

SIRS202 = read.xlsx("Total Cohort - Assessment Detail.xlsx")
str(SIRS202)

summary(as.factor(Workbook$`Cohort.Year.(year.1st.entered.9th)`[grepl("yes", Workbook$`Included.in.Graduation.Cohort?`, T)]))

