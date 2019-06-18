# PSATScores.R
# Prepares the PSAT scores from (data-drive)\SAT's\college_board_data.xlsx to load into the workbook

#### Start with MainScript.R, then come back to this. ####

# Make a copy of the PSAT object
PSATraw.xlsx <- PSAT.raw

# Ensure that the data is of a numeric type. Empty cells can cause them to be read as character vectors.
PSATraw.xlsx$Read  <- as.integer(PSATraw.xlsx$Read)
PSATraw.xlsx$Math  <- as.integer(PSATraw.xlsx$Math)
PSATraw.xlsx$Write <- as.integer(PSATraw.xlsx$Write)

# Add the subscores together to get the total score
PSATraw.xlsx$Score <- rowSums(PSATraw.xlsx[,c("Read","Math","Write")], na.rm=TRUE)

# Get cohort data
PSATraw.xlsx$Cohort <- as.integer(Workbook$`Cohort.Year.(year.1st.entered.9th)`[match(PSATraw.xlsx$ID, Workbook$`Local.ID.(optional)`)])
if(sum(is.na(PSATraw.xlsx$Cohort)) > 0){
  print("Warning!  A student has a bad ID.")
  print(PSATraw.xlsx[is.na(PSATraw.xlsx$Cohort),])
} else {
  print("Yay!  No messed up student ID's.")
}

# Sort by Cohort, ID, then Score (descending)
PSATsort <- PSATraw.xlsx[with(PSATraw.xlsx, order(Cohort, ID, -Score)),]

# Add rank of score by student
PSATsort$Rank[1] <- 1
for (i in 2:length(PSATsort$ID)) {
  if (PSATsort$ID[i] == PSATsort$ID[i-1]) {
    PSATsort$Rank[i] <- PSATsort$Rank[i-1] + 1
  }
  else {
    PSATsort$Rank[i] <- 1
  }
}

# Remove rows that aren't the best or second best scores
PSATsort <- subset(PSATsort, (PSATsort$Rank > 0) & (PSATsort$Rank < 3))

# Sort by Cohort, ID, then Date
PSATsort <- PSATsort[with(PSATsort, order(Cohort, ID, Year)),]

# Change rank of score to indicate chronological order
PSATsort$Rank[1] <- 1
for (i in 2:length(PSATsort$ID)) {
  if (PSATsort$ID[i] == PSATsort$ID[i-1]) {
    PSATsort$Rank[i] <- PSATsort$Rank[i-1] + 1
  }
  else {
    PSATsort$Rank[i] <- 1
  }
}

# Separate first and second scores
PSAT1 <- subset(PSATsort, PSATsort$Rank == 1)
PSAT2 <- subset(PSATsort, PSATsort$Rank == 2)


# Create a CSV for each cohort year with best and second best PSAT scores (as needed)
newFrame <- NULL

# Create a dataframe that matches the order of students in the workbook
newFrame <- data.frame(ID = Workbook$`Local.ID.(optional)`)

# Add first PSATs
newFrame$Read1  <- as.integer(PSAT1$Read[match(newFrame$ID, PSAT1$ID)])
newFrame$Math1  <- as.integer(PSAT1$Math[match(newFrame$ID, PSAT1$ID)])
newFrame$Write1 <- as.integer(PSAT1$Write[match(newFrame$ID, PSAT1$ID)])
newFrame$Year1  <- as.integer(PSAT1$Year[match(newFrame$ID, PSAT1$ID)])

# Add second  PSATs
newFrame$Read2  <- as.integer(PSAT2$Read[match(newFrame$ID, PSAT2$ID)])
newFrame$Math2  <- as.integer(PSAT2$Math[match(newFrame$ID, PSAT2$ID)])
newFrame$Write2 <- as.integer(PSAT2$Write[match(newFrame$ID, PSAT2$ID)])
newFrame$Year2  <- as.integer(PSAT2$Year[match(newFrame$ID, PSAT2$ID)])

# Compare to existing scores
cohort.xlsx = Workbook[,c("Local.ID.(optional)","PSAT.Read.1", "PSAT.Math.1", "PSAT.Write.1", "PSAT.Year.1", "PSAT.Read.2", "PSAT.Math.2", "PSAT.Write.2", "PSAT.Year.2")]
for(j in 2:ncol(cohort.xlsx)){ cohort.xlsx[,j] = as.integer(cohort.xlsx[,j]) }
comparison1 = MbetterComp(as.matrix(newFrame[,2:9]), as.matrix(cohort.xlsx[,2:9]))
if(!(all(comparison1))){
  entries = which(!comparison1, arr.ind = T)
  entryRows = unique(entries[,1])
  newFrame[entryRows,]
  comparison2 = MbetterGreater(as.matrix(newFrame[,2:9]), as.matrix(cohort.xlsx[,2:9])) + comparison1  
  if(any(comparison2 == 0)){
    print(paste0("There are PSAT scores in the the workbook that are better than what has been generated."))
    entries = which(comparison2 == 0, arr.ind = T)
    entryRows = unique(entries[,1])
    newFrame[entryRows,]
  } else {
    # Write output to a CSV
    write.csv(
      x         = newFrame,
      file      = paste0(OutFolder, "PSAT_Scores.csv"),
      na        = "",
      row.names = FALSE)  
  }
} # /if the generated scores are different from the existing scores 



# At this point, paste from the CSVs to the Accountability Workbook. 
# The only cohorts that need updating are the ones for which CSVs were generated
# If no CSVs were generated, nothing needs to be updated
# If there are any messages printed about existing scores being better than what was generated, look at those carefully.

