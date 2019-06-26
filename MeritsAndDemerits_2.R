# Merits and demerits 2

#-----------------------------#
#### Get everything set up ####
#-----------------------------#

# Enter the url of the sheet
thisURL = "https://docs.google.com/spreadsheets/d/1HyuQh5_1ZpQzrM8VkgtZ2dVekU3Qp5WqpC9WNPNvdGU/edit?usp=sharing"  

tabs2ignore = c()                                                        # should any tabs be ignored for now?
theSheet = gs_url(thisURL, verbose = F)                                                        # identify the sheet
Sheetpath = gs_download(from = theSheet, to = paste0(OutFolder, "mdwkbk.xlsx"), overwrite = T) # download it as an excel file
mdworkbook = openxlsx::loadWorkbook(Sheetpath)                                                 # load it
dataTabs = openxlsx::getSheetNames(Sheetpath)                                                  # identify the tabs
dataTabs = dataTabs[!(dataTabs %in% tabs2ignore)]                                              # remove any that are not needed
demeritList = vector(mode = "list", length = length(dataTabs))                                 # create a list to hold the demerits
names(demeritList) = dataTabs                                                                  # name the list elements
meritList = demeritList                                                                        # create a list to hold the merits


#--------------------------------#
#### Load data into the lists ####
#--------------------------------#

for(thisTabName in dataTabs){
  print(paste0("working on ", thisTabName))
  thisTab = read.xlsx(xlsxFile = mdworkbook, sheet = thisTabName, colNames = F)
  thisHeader = thisTab[1:2,]
  
  for(i in 2:ncol(thisHeader)){
    if(is.na(thisHeader[1,i])){
      thisHeader[1,i] = thisHeader[1,i - 1]
    }
  }
  
  thisHeader[3,] = NA
  thisHeader = DFna.to.empty(thisHeader)
  for(i in 1:ncol(thisHeader)){
    thisHeader[3,i] = paste0(thisHeader[1,i], "_", thisHeader[2,i])
  }
  thisTab = thisTab[-(1:2),]
  thisTab = DFna.to.empty(thisTab)
  colnames(thisTab) = unname(unlist(thisHeader[3,]))
  ColumnTypes = unname(unlist(thisHeader[2,]))
  
  for(thisColumnNumber in 1:ncol(thisTab)){
    if(ColumnTypes[thisColumnNumber] %in% c("Merits", "Demerits")){
      thisColumn = thisTab[,thisColumnNumber]
      tempColumn = SWSM(as.numeric(thisColumn))
      if(any(thisColumn[is.na(tempColumn)] != "")){ # if any values cannot be converted to numbers and are not "", throw an error
        warning(paste0("Problem with the ", colnames(thisTab)[thisColumnNumber], " column in the ", thisTabName , " tab."))
      }
      thisTab[,thisColumnNumber] = tempColumn
    }
  }
  
  thisMerits = thisTab[,ColumnTypes %in% c("","Merits")]
  thisDemerits = thisTab[,ColumnTypes %in% c("","Demerits")]  
  demeritList[[thisTabName]] = thisDemerits
  meritList[[thisTabName]] = thisMerits
}


#------------------------------------#
#### Check for errors in demerits ####
#------------------------------------#
i = 20
for(i in 1:length(demeritList)){
  print(paste0("Examing tab ", names(demeritList)[i]))
  curSheet = demeritList[[i]]
  
  if(!("Student #_" %in% colnames(curSheet))){
    print(paste0(names(demeritList)[i], " has no 'Student #' column"))
  } else if(any(grepl(pattern = "E", x = curSheet$`Student #_`))){
    print(paste0(names(demeritList)[i], " has a 'Student #' column that is not formatted as character"))
  }
  
  if("X1" %in% colnames(curSheet)){
    print(paste0(names(demeritList)[i], " has an X1 column"))
  }
  
  if("V1" %in% colnames(curSheet)){
    print(paste0(names(demeritList)[i], " has a V1 column"))
  }
  
  if(ncol(curSheet) < 5){
    print(paste0(names(demeritList)[i], " has very few columns, like only ", ncol(curSheet)))
  } else {
    highestValue = betterMax(curSheet[,4:ncol(curSheet)])
    if(is.na(highestValue)){
      print(paste0(names(demeritList)[i], " has no demerits!?"))
    } else if(highestValue > 99){
      
      print(paste0("  ", names(demeritList)[i], " has demerit values that are really big, like ", highestValue))
      tempSheet = curSheet == highestValue
      columnsToReport = which(tempSheet, arr.ind = T)
      columnsToReport = colnames(tempSheet)[columnsToReport[,2]]
      print(paste0("  Check columns: ", VectorSentence(x = columnsToReport)))
      
    }
  }
}


#----------------------------------#
#### Check for errors in merits ####
#----------------------------------#

for(i in 1:length(meritList)){
  
  curSheet = meritList[[i]]
  
  if(!("Student #_" %in% colnames(curSheet))){
    print(paste0(names(meritList)[i], " has no 'Student #' column"))
  } 
  if("X1" %in% colnames(curSheet)){
    print(paste0(names(meritList)[i], " has an X1 column"))
  }
  
  allValues = unname(unlist((curSheet[,4:ncol(curSheet)])))
  highestValue = betterMax(allValues)
  allValues.valid = allValues[!is.na(allValues)]
  allValues.positive = allValues.valid[allValues.valid > 0]
  lowestValue = min(allValues.positive)
  
  if(is.na(highestValue)){
    print(paste0(names(meritList)[i], " has no merits!?"))
  } else if(lowestValue < 20){
    for(thisColumnNumber in 4:ncol(curSheet)){
      thisColumnName = colnames(curSheet)[thisColumnNumber]
      colValues = unname(unlist(curSheet[thisColumnNumber]))
      colValues.valid = colValues[!is.na(colValues)]
      colValues.positive = colValues.valid[colValues.valid > 0]
      if(length(colValues.positive) > 0){
        colLowestValue = min(colValues.positive)
        if(colLowestValue < 20){
          print(paste0("In tab ",names(meritList)[i], 
                       ", column ", thisColumnName ,
                       " has merit values that are really low, like ", colLowestValue)) 
        }
      }
    } # /for
  } # /if-elseif
}


#---------------------------------------#
#### Aggregate and organize demerits ####
#---------------------------------------#

# Create a list containing all demerit totals by student by week
demeritTotals = vector(mode = "list", length = length(demeritList))  # set up a list to hold demerit totals
for(i in 1:length(demeritList)){
  x = demeritList[[i]]                                               # Get the current demerit spreadsheet
  print(paste0("Sheet ",i, " - ", names(demeritList)[i]))            # Print the counter and the name of the tab
  demeritTable = data.frame(                                         # Collapse it to get sums of demerits
    X1 = apply(X = x[,4:ncol(x)], MARGIN = 1, FUN = sum, na.rm = T)) 
  colnames(demeritTable) = names(demeritList)[i]                     # Add a column name using the date
  rownames(demeritTable) = x$`Student #_`                            # Add row names using student ID's
  demeritTotals[[i]] = demeritTable                                  # load it into the demeritTotals list
  names(demeritTotals)[i] = names(demeritList)[i]                    # Name the list element using the date
} # /for



# For each data.frame in the demeritTotals list, transpose it so that there is one column per student
for(i in 1:length(demeritTotals)){ demeritTotals[[i]] = as.data.frame.matrix(t(demeritTotals[[i]])) }


# Combine the demerit totals into a single data.frame with one column per student and one row per week
AllDemeritTotals = as.data.frame(rbindlist(l = demeritTotals, use.names = T, fill = T, idcol = "Week"))

if(ncol(AllDemeritTotals) > 1.5 * ncol(demeritTotals[[2]])){
  print("Something is wrong with the student IDs.")
}

#----------------------------------------------#
#### Calculate consecutive weeks by student ####
#----------------------------------------------#

Consecutive = integer(0)                                         # initialize vector of max consecutive weeks of 0 demerits by student
for(i in 2:ncol(AllDemeritTotals)){                              # for each column after the first one (ie for each student)
  entries = AllDemeritTotals[,i]                                 # Get the entries for that column (# demerits by week for that student)
  anyDemeritsByWeek = as.integer(VbetterComp(entries,0))         # Determine which weeks had 0 demerits
  DemeritString = paste0(anyDemeritsByWeek, collapse = "")       # Collapse it into a string of 1's and 0's
  y = strsplit(x = DemeritString, split = "0", fixed = T)[[1]]   # Get strings of consecutive 1's (consecutive weeks with 0 demerits)
  Consecutive = c(Consecutive,max(nchar(y)))                     # Get length of longest run of 0 demerits and append to Consecutive
} # /for

Consecutive = data.frame(consecWeeks = Consecutive)                                      # Convert it to a data.frame
Consecutive$ID = as.numeric(colnames(AllDemeritTotals)[-1])                              # Add student ID's
Consecutive$LastName =   Workbook$Last[match(Consecutive$ID, Workbook$`Local.ID`)]       # Add last name
Consecutive$FirstName =  Workbook$First[match(Consecutive$ID, Workbook$`Local.ID`)]      # Add first name
Consecutive$GradeLevel = Workbook$`Grade.(l`[match(Consecutive$ID, Workbook$`Local.ID`)] # add grade level
Consecutive = Consecutive[!is.na(Consecutive$GradeLevel),]


#--------------------------#
#### Summarize the info ####
#--------------------------#

summary(as.factor(Consecutive$consecWeeks))
summary(Consecutive)
sqrt(var(Consecutive$consecWeeks))
ggplot(data = Consecutive, mapping = aes(consecWeeks)) +         # plot no. of students by length of consecutive demeritless weeks
  geom_histogram(bins = max(Consecutive$consecWeeks) +1, col="red") + 
  labs(y = "Number of Students")


#-----------------------#
#### Generate output ####
#-----------------------#

Consecutive = Consecutive[order(Consecutive$consecWeeks, decreasing = T),]
write.csv(x = Consecutive, file = paste0(OutFolder, "ConsecutiveWeeksOfNoDemerits.csv"), row.names = F)


#--------------------------#
#### Do some more stuff ####
#--------------------------#

# Get the total number of demerits per student
TotalDemeritsByStudent = apply(X = AllDemeritTotals[2:ncol(AllDemeritTotals)], MARGIN = 2, FUN = sum, na.rm = T) 
summary(TotalDemeritsByStudent)

TotalDemeritsByStudent[TotalDemeritsByStudent == max(TotalDemeritsByStudent)]
which(TotalDemeritsByStudent == max(TotalDemeritsByStudent))

# Max demerit earner info
# This checks to see whether there are any individual entries in a demerit spreadsheet that are just crazy
# The output is the largest number of demerits assigned by one teacher in each week to the student with the most overall demerits
maxEarner = names(TotalDemeritsByStudent[TotalDemeritsByStudent == max(TotalDemeritsByStudent)]) # ID's of the max demerit earner(s) 
for(i in 1:length(demeritList)){
  x = demeritList[[i]]
  weekname = names(demeritList)[i]
  if(any(x$`Student #_` == maxEarner)){
    maxEarned = max(x[x$`Student #_` == as.integer(maxEarner),4:ncol(x)],na.rm = T)
    if(!is.na(maxEarned)){
      print(paste0(c("Entry: ", i, ",  Week: ", weekname, ",  Highest Demerit Entry: ", maxEarned), collapse = ""))
    }
  }
}

