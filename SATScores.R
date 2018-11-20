# SATScores.R
# Prepares the SAT scores from (data-drive)\SAT's\college_board_data.xlsx
# This script is designed to update the old accountability workbook

#### Start with MainScript.R, then come back to this. ####

# Make a copy of the data
SATraw.xlsx <- SAT.raw

# Ensure that the data is of a numeric type. Empty cells can cause them to be read as character vectors.
SATraw.xlsx$Reading  <- as.integer(SATraw.xlsx$Reading)
SATraw.xlsx$Math     <- as.integer(SATraw.xlsx$Math)
SATraw.xlsx$Writing  <- as.integer(SATraw.xlsx$Writing)

# Add the subscores together to get the total score
SATraw.xlsx$Score <- rowSums(SATraw.xlsx[,c("Reading","Math","Writing")], na.rm = TRUE)

# Get cohort data
SATraw.xlsx$Cohort <- as.integer(Workbook$`Cohort.Year.(year.1st.entered.9th)`[match(SATraw.xlsx$ID, Workbook$`Local.ID.(optional)`)])

# Sort by Cohort, ID, then Score (descending)
SATsort <- SATraw.xlsx[with(SATraw.xlsx, order(Cohort, ID, -Score)),]

# Add rank of score by student
SATsort$Rank[1] <- 1
for (i in 2:length(SATsort$ID)) {
  if (SATsort$ID[i] == SATsort$ID[i-1]) {
    SATsort$Rank[i] <- SATsort$Rank[i-1] + 1
  }
  else {
    SATsort$Rank[i] <- 1
  }
}

# Remove rows that aren't the best or second best scores
SATsort <- subset(SATsort, (SATsort$Rank > 0) & (SATsort$Rank < 3))

# Sort by Cohort, ID, then Date
SATsort <- SATsort[with(SATsort, order(Cohort, ID, Year, Month)),]

# Change rank of score to indicate chronological order
SATsort$Rank[1] <- 1
for (i in 2:length(SATsort$ID)) {
  if (SATsort$ID[i] == SATsort$ID[i-1]) {
    SATsort$Rank[i] <- SATsort$Rank[i-1] + 1
  }
  else {
    SATsort$Rank[i] <- 1
  }
}

# Separate first and second scores
SAT1 <- subset(SATsort, SATsort$Rank == 1)
SAT2 <- subset(SATsort, SATsort$Rank == 2)

# Get a list of years
Years <- c(2006:schoolYear())

# Create a CSV for each cohort year with best and second best SAT scores
newFrame <- NULL
for (i in Years) {
  print(i)
  # Get the sheet with the cohort
  cohort.xlsx <- Workbook[Workbook$`Cohort.Year.(year.1st.entered.9th)` == i,]
  
  # Create a matching dataframe
  newFrame <- data.frame(ID = cohort.xlsx$`Local.ID.(optional)`)
  
  # Add first SATs
  newFrame$Read1  <- as.integer(SAT1$Reading[match(newFrame$ID, SAT1$ID)])
  newFrame$Math1  <- as.integer(SAT1$Math[match(newFrame$ID, SAT1$ID)])
  newFrame$Write1 <- as.integer(SAT1$Writing[match(newFrame$ID, SAT1$ID)])
  newFrame$Total1 <- as.integer(SAT1$Score[match(newFrame$ID, SAT1$ID)])
  newFrame$Month1 <- as.integer(SAT1$Month[match(newFrame$ID, SAT1$ID)])
  newFrame$Year1  <- as.integer(SAT1$Year[match(newFrame$ID, SAT1$ID)])
  
  # Add second  SATs
  newFrame$Read2  <- as.integer(SAT2$Reading[match(newFrame$ID, SAT2$ID)])
  newFrame$Math2  <- as.integer(SAT2$Math[match(newFrame$ID, SAT2$ID)])
  newFrame$Write2 <- as.integer(SAT2$Writing[match(newFrame$ID, SAT2$ID)])
  newFrame$Total2 <- as.integer(SAT2$Score[match(newFrame$ID, SAT2$ID)])
  newFrame$Month2 <- as.integer(SAT2$Month[match(newFrame$ID, SAT2$ID)])
  newFrame$Year2  <- as.integer(SAT2$Year[match(newFrame$ID, SAT2$ID)])
  
  
  # Compare to existing scores
  cohort.xlsx = cohort.xlsx[,c("Local.ID.(optional)",
                               "SAT.Read.1", "SAT.Math.1", "SAT.Write.1", "SAT.Total.1", "SAT.Month.1", "SAT.Year.1",
                               "SAT.Read.2", "SAT.Math.2", "SAT.Write.2", "SAT.Total.2", "SAT.Month.2", "SAT.Year.2")]
  for(j in 2:ncol(cohort.xlsx)){ cohort.xlsx[,j] = as.integer(cohort.xlsx[,j]) }
  comparison1 = MbetterComp(as.matrix(newFrame[,2:13]), as.matrix(cohort.xlsx[,2:13]))
  if(!(all(comparison1))){
    entries = which(!comparison1, arr.ind = T)
    entryRows = unique(entries[,1])
    newFrame[entryRows,]
    comparison2 = MbetterGreater(as.matrix(newFrame[,2:13]), as.matrix(cohort.xlsx[,2:13])) + comparison1  
    if(any(comparison2 == 0)){
      print(paste0("There are PSAT scores in the ", i , " tab of the workbook that are better than what has been generated."))
      entries = which(comparison2 == 0, arr.ind = T)
      entryRows = unique(entries[,1])
      newFrame[entryRows,]
    } else {
      # Write output to a CSV
      write.csv(
        x         = newFrame,
        file      = paste0(OutFolder, as.character(i), "Cohort_SAT_Scores.csv"),
        na        = "",
        row.names = FALSE)
    }
  } # /if the generated scores are different from the existing scores 
  
  # Cleanup, prep for next pass
  cohort.xlsx <- NULL
  newFrame    <- NULL
}

# At this point, paste from the CSVs to the Accountability Workbook. 
# The only cohorts that need updating are the ones for which CSVs were generated
# If no CSVs were generated, nothing needs to be updated
# If there are any messages printed about existing scores being better than what was generated, look at those carefully.




