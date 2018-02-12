#functions.R

library(abind)
library(ggplot2)
library(scales)
library(reshape2)
library(openxlsx)
library(dBtools)
library(googlesheets)
library(stringr)
SWSM(library(data.table, quietly = T, verbose = F))

# Location of the college board workbook
CollegeBoardLocation = "\\\\stuthin2/Data/SAT's/college_board_data.xlsx"

# Location of the CSI accountability workbook
AcctWkBkLocation = "\\\\stuthin2\\Data\\Accountability Spreadsheet\\working copy\\Green Tech Cohort Data Collection Workbook.xlsx"

# Location of the eScholar templates for the SIRS exports
TemplateLocation = "\\\\stuthin2/Data/SIRS manuals templates guides etc/2017-18eScholarTemplatesNYS_2017-09-25.xlsx"

# URL of the Course-Subject alignment table
CourseSubjectAddress = "https://docs.google.com/a/greentechhigh.org/spreadsheets/d/17QhVYZkjbx34M6wBvtHUYa_XrRUlRbOtuOsQ4P5l-nk/edit?usp=sharing"

# Location of the PowerSchool exports file
PSLocation = "PowerSchoolAll.xlsx"

# Location of the Regents Database export file
RDBLocation = "RegentsDB.csv"

# Location of direct cert files from NYSSIS
LunchLocation = "\\\\stuthin2/data/2017-2018\\lunch"

# Location of output files
OutFolder = paste0(getwd(), "/Temporary Output/")