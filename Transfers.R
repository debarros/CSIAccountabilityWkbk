# Transfers.R

# This script has somethign to do with figuring out which students transfered out, when, why, and to where.

#Read in the enrollment files from the past n years
n = 2
enrollment = vector(mode = "list", length = n)
enrollment[[1]] = read.csv("enrollment1415.csv", stringsAsFactors = F, header = F)
enrollment[[2]] = read.csv("enrollment1516.csv", stringsAsFactors = F, header = F)

str(enrollment, max.level = 1)

y = read.xlsx(wblocation, sheet = 11, startRow = 2)[,3] #get the column names from the template workbook

#set the number of columns to match the number of column names
for(i in 1:n){
  ColsToAdd = length(y) - ncol(enrollment[[i]])
  if(ColsToAdd > 0){
    enrollment[[i]][,ncol(enrollment[[i]]) + 1:ColsToAdd] = NA
  }
}

#Use the nysed template to assign the column names to the enrollment files
enr2 = lapply(X = enrollment, FUN = `names<-`, value = y)
str(enr2)

#combine into one dataframe
enr3 = do.call("rbind.data.frame", enr2)
str(enr3)

#use the exit codes to limit the data set to just students who had a particular kind of exit (170 is transfer to NYS public school)
codesused = unique(enr3$`SCHOOL EXIT TYPE CODE (REASON FOR ENDING ENROLLMENT CODE)`)
exitcodes = read.csv("ExitCodes.csv", colClasses = "character")
exitcodes$Reason[exitcodes$Code %in% codesused]
enr4 = enr3[enr3$`SCHOOL EXIT TYPE CODE (REASON FOR ENDING ENROLLMENT CODE)` == 170,]
enr4 = enr4[!is.na(enr4$`SCHOOL EXIT TYPE CODE (REASON FOR ENDING ENROLLMENT CODE)`),]


