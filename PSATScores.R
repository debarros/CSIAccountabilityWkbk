# PSATScores.R
# Prepares the PSAT scores from (data-drive)\SAT's\college_board_data.xlsx

#### Start with MainScript.R, then come back to this. ####

# Read the SAT file for PSAT data
PSATraw.xlsx <- openxlsx::read.xlsx(
  xlsxFile = "J:/SAT's/college_board_data.xlsx",
  sheet = "PSAT",
  startRow = 1,
  na.strings = c(""))

# Ensure that the data is of a numeric type. Empty cells can cause them to be read as character vectors.
PSATraw.xlsx$Read  <- as.integer(PSATraw.xlsx$Read)
PSATraw.xlsx$Math  <- as.integer(PSATraw.xlsx$Math)
PSATraw.xlsx$Write <- as.integer(PSATraw.xlsx$Write)

# Add the subscores together to get the total score
PSATraw.xlsx$Score <- rowSums(PSATraw.xlsx[,c("Read","Math","Write")], na.rm=TRUE)

# Get cohort data
PSATraw.xlsx$Cohort <- as.integer(Workbook$`Cohort.Year.(year.1st.entered.9th)`[match(PSATraw.xlsx$ID, Workbook$`Local.ID.(optional)`)])

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

# Get a list of years
Years <- c(2006:as.integer(format(Sys.Date(), "%Y")))

# Create a CSV for each cohort year with best and second best PSAT scores
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
  
  # Write output to a CSV
  write.csv(
    x         = newFrame,
    file      = paste0(as.character(i), "Cohort_PSAT_Scores.csv"),
    na        = "",
    row.names = FALSE)
  
  # Cleanup, prep for next pass
  cohort.xlsx <- NULL
  newFrame    <- NULL
}

#### At this point, paste from the CSVs to the Accountability Workbook. ####
#### Make sure not to erase data that is already there!!!! ####