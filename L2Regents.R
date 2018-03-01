# L2Regents.R

# This script compares scores from the accountability workbook to scores from the SIRS-341 report
# As always, run Mainscript first.
# It might also be a good idea to run the L2cohorts.R script first.

# Download the detail report (Excel 2007 Data) of the SIRS-340 

SIRS341 = read.xlsx(file.choose())                                                  # load the SIRS 341 export
rownames(SIRS341) = SIRS341$STUDENT_ID                                              # use student ID as rownames
cols2use = grep(pattern = "SCORE", x = colnames(SIRS341), value = T)                # find the columns that have regents scores
SIRS341 = SIRS341[,cols2use]                                                        # subset to just those columns
colnames(SIRS341) = testLookup$Code[match(colnames(SIRS341), testLookup$SIRS_341)]  # switch the column names to ones that match prior code
SIRS341 = SIRS341[,colnames(CompareMatrix)]                                         # reorder the columns to match the matrix of existing scores
stud2use = intersect(rownames(CompareMatrix), rownames(SIRS341))                    # determine the set of students to use
SIRS341 = SIRS341[stud2use,]                                                        # subset to those students
CompareMatrix2 = CompareMatrix[stud2use,]                                           # subset the existing scores to those students

identical(dimnames(SIRS341),dimnames(CompareMatrix2))                               # check to make sure the dimnames are the same for the two matrices

# create an array to compare local scores to SED scores
# the rows are students, the columns are exams, and the planes are SED scores, Local scores, and Comparison of scores
CompareArray = abind::abind(SIRS341, CompareMatrix2, MbetterComp(SIRS341, CompareMatrix2), along = 3) 

# Limit the comparison array to just those entries that don't match
CompareArray = CompareArray[apply(CompareArray[,,3], 1, sum) < dim(CompareArray)[2],,]
CompareArray = CompareArray[,apply(CompareArray[,,3], 2, sum) < dim(CompareArray)[1],]


Issues = which(CompareArray[,,3] == 0, arr.ind = T)                         # Get row and column numbers for nonmatches
rownames(Issues) = NULL
Issues = as.data.frame(Issues)                                              # Convert to a data.frame
Issues$StudentID = dimnames(CompareArray)[[1]][Issues$row]                  # add student ID's
Issues$Exam = dimnames(CompareArray)[[2]][Issues$col]                       # Add exam names
Issues[,c("SIRS.341.Score", "Local.Score")] = NA_real_                      # Initialize score fields
for(i in 1:nrow(Issues)){                                                   # Load the scores
  Issues$SIRS.341.Score[i] = CompareArray[Issues$row[i], Issues$col[i], 1]
  Issues$Local.Score[i] = CompareArray[Issues$row[i], Issues$col[i], 2]
}


Issues$SEDbetter = VbetterGreater(Issues$SIRS.341.Score, Issues$Local.Score)
Issues[!Issues$SEDbetter,]

Issues$LastName = Workbook$Last.Name[match(Issues$StudentID, Workbook$Local.ID)]
Issues$FirstName = Workbook$First.Name[match(Issues$StudentID, Workbook$Local.ID)]

write.csv(x = Issues, file = paste0(OutFolder, "Regents Scores - SIRS vs Local.csv"))


