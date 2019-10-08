#functions.R

library(dBtools)
library(abind)
SWSM(library(ggplot2, quietly = T, verbose = F))
library(scales)
library(reshape2)
library(openxlsx)
library(googlesheets)
library(stringr)
library(googledrive)
SWSM(library(data.table, quietly = T, verbose = F))

# Location of the college board workbook
CollegeBoardLocation = "\\\\stuthin2/Data/SAT's/college_board_data.xlsx"

# Location of the CSI accountability workbook
AcctWkBkLocation = "\\\\stuthin2\\Data\\Accountability Spreadsheet\\working copy\\GTH Data Collection Workbook.xlsx"

# Location of the eScholar templates for the SIRS exports
TemplateLocation = "\\\\stuthin2/Data/SIRS manuals templates guides etc/2019-20eSCHOLARTEMPLATES-07-26-2019.xlsx"

# URL of the Course-Subject alignment table
CourseSubjectAddress = "https://docs.google.com/a/greentechhigh.org/spreadsheets/d/17QhVYZkjbx34M6wBvtHUYa_XrRUlRbOtuOsQ4P5l-nk/edit?usp=sharing"

# URL of the nightly student csv export
CurrentStudentsAddress = "https://drive.google.com/file/d/1EnnUpm2B-CZ1zYDyYoml6RLKi_o47Xky/view?usp=sharing"

# Location of the PowerSchool exports file
PSLocation = "PowerSchoolAll.xlsx"

# Location of the Regents Database export file
RDBLocation = "RegentsDB.csv"

# Location of direct cert files from NYSSIS
LunchLocation = "\\\\stuthin2/data/2019-2020\\lunch"

# Location of output files
OutFolder = paste0(getwd(), "/Temporary Output/")