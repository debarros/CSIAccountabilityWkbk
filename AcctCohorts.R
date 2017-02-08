#AcctCohorts.R

#Figure out cohort membership by year by cohort

#limit the workbook to just those students who are in the acct cohort for the year
#Change the variable name to reflect the year it was in september
Workbook1 = Workbook[Workbook$X2014 == "yes",] 

#Create logical variables indicating whether the student passed
Workbook1$MathPass = Workbook1$Highest.Math.Score >= 80
Workbook1$ELAPass = Workbook1$Highest.ELA.Score >= 75
Workbook1$SciPass = Workbook1$Highest.Science.Score >= 65
Workbook1$GlobPass = Workbook1$GlobalScaled.Score >= 65
Workbook1$UsPass = Workbook1$US.HistoryScaled.Score >= 65

#Subset the workbook to just the relevant columns
Workbook2 = Workbook1[,c(13,165:169)]

#replace NA's with FALSE
Workbook2[which(is.na(Workbook2), arr.ind = TRUE)] = FALSE


x = list()  #initialize the list
for(i in 2:6){                        #for each exam category
  x[[i-1]] = table(Workbook2[,c(1,i)]) #store a frequency table of Cohort x Passed for that category
}


for (i in 1:5){                               #for each category
  w = names(dimnames(x[[i]]))                 #grab the variable names
  y = rownames(x[[i]])                        #grab the list of cohorts
  x[[i]] = array(data = x[[i]], dim = c(6,3)) #convert it to an array with an extra column
  x[[i]][,3] = x[[i]][,1] + x[[i]][,2]        #put the cohort totals in the extra column
  x[[i]][,1] = as.integer(y)                  #put the cohort years in the first column
  colnames(x[[i]]) = c(w,"Total")             #add column names
}



names(Workbook1)

MathPI = table(Workbook1[,c(13,153)])         
ElaPI = table(Workbook1[,c(13,145)])

write.csv(x,"PassRates.csv")                  #export the data
write.csv(MathPI,"MathPI.csv")
write.csv(ElaPI,"ElaPI.csv")
