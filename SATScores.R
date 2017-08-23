# SATScores.R
# Prepares the SAT scores from (data-drive)\SAT's\college_board_data.xlsx

#### Start with MainScript.R, then come back to this. ####

# Read the SAT file for SAT data
SATraw.xlsx <- openxlsx::read.xlsx(
  xlsxFile = "J:/SAT's/college_board_data.xlsx",
  sheet = "SAT",
  startRow = 1,
  na.strings = c(""))

# Ensure that the data is of a numeric type. Empty cells can cause them to be read as character vectors.
SATraw.xlsx$Reading  <- as.integer(SATraw.xlsx$Reading)
SATraw.xlsx$Math     <- as.integer(SATraw.xlsx$Math)
SATraw.xlsx$Writing  <- as.integer(SATraw.xlsx$Writing)

# Add the subscores together to get the total score
SATraw.xlsx$Score <- rowSums(SATraw.xlsx[,c("Reading","Math","Writing")], na.rm=TRUE)

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
SATsort <- SATsort[with(SATsort, order(Cohort, ID, Year)),]

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
Years <- c(2006:as.integer(format(Sys.Date(), "%Y")))

# Create a CSV for each cohort year with best and second best SAT scores
newFrame <- NULL
for (i in Years) {
  i
  # Get the sheet with the cohort
  sheetName <- paste0(as.character(i), " Cohort")
  
  cohort.xlsx <- openxlsx::read.xlsx(
    xlsxFile = "J:/Accountability Spreadsheet/working copy/Green Tech Cohort Data Collection Workbook.xlsx", 
    sheet    = sheetName,
    colNames = TRUE,
    startRow = 2)
  
  # Create a matching dataframe
  newFrame$ID <- cohort.xlsx$`Local.ID.(optional)`
  
  # Add first SATs
  newFrame$Read1  <- as.integer(SAT1$Reading[match(newFrame$ID, SAT1$ID)])
  newFrame$Math1  <- as.integer(SAT1$Math[match(newFrame$ID, SAT1$ID)])
  newFrame$Write1 <- as.integer(SAT1$Writing[match(newFrame$ID, SAT1$ID)])
  newFrame$Year1  <- as.integer(SAT1$Year[match(newFrame$ID, SAT1$ID)])
  
  # Add second  SATs
  newFrame$Read2  <- as.integer(SAT2$Reading[match(newFrame$ID, SAT2$ID)])
  newFrame$Math2  <- as.integer(SAT2$Math[match(newFrame$ID, SAT2$ID)])
  newFrame$Write2 <- as.integer(SAT2$Writing[match(newFrame$ID, SAT2$ID)])
  newFrame$Year2  <- as.integer(SAT2$Year[match(newFrame$ID, SAT2$ID)])
  
  # Write output to a CSV
  write.csv(
    x         = newFrame,
    file      = paste0(as.character(i), "Cohort_SAT_Scores.csv"),
    na        = "",
    row.names = FALSE)
  
  # Cleanup, prep for next pass
  cohort.xlsx <- NULL
  newFrame    <- NULL
}

#### At this point, paste from the CSVs to the Accountability Workbook. ####
#### Make sure not to erase data that is already there!!!! ####