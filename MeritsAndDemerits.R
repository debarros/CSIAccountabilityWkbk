# Merits and demerits

# Generate a table of the URL's of all the demerit spreadsheets ####
# Hopefully there is a faster way to do this that doesn't involve getting each url one at a time

urlTable = matrix(
  data = c(
    "5/24/17",  "https://docs.google.com/spreadsheets/d/1H6DnqfFcIhHwaiQ_EX6t-j9hFJAXjVRw_USKdCtUwMg/edit",
    "5/18/17",  "https://docs.google.com/spreadsheets/d/1D1SrHoRIVDlLkuXljhgFBR4NPU9ZZYz3RqY0_Y4dOF4/edit",
    "4/6/17",   "https://docs.google.com/spreadsheets/d/1_AozER7Vm12ndvkWc2Bppa6E3IqDu2KQvGkZf-2oWIg/edit",
    "4/12/17",  "https://docs.google.com/spreadsheets/d/124x6kMZa0XXnzHwVIOCGyt3H7uC88T38811XE3QqqrA/edit",
    "4/27/17",  "https://docs.google.com/spreadsheets/d/1RvH2Ormj82-nxWDg87JYJTJVRKnWNnHTLuMMvcVnh0U/edit",
    "12/1/16",  "https://docs.google.com/spreadsheets/d/1sNSb1jRcFzIX9Bz1mKkod-KSpyuTxhq69m7Y8rbt4tQ/edit",
    "12/15/16", "https://docs.google.com/spreadsheets/d/1VG9Bk66y2WmLbF5R3DwAuw5PdHACnxIbz0Twg1IxLhM/edit",
    "12/21/16", "https://docs.google.com/spreadsheets/d/1o8YrfiS8aGKqGHOc2HEQxjNczK0bkPbp9w30KkLdPuY/edit",
    "12/8/16",  "https://docs.google.com/spreadsheets/d/1nLFlhIe9hYGvsoi9I4XDggqMSnveEg3rHv7q5ITpsJ4/edit",
    "2/16/17",  "https://docs.google.com/spreadsheets/d/1wzyoxWakFOUZBU_a9bjF6K6KifzWybgyZYAPaiG9Ro4/edit",
    "2/2/17",   "https://docs.google.com/spreadsheets/d/1Gb_E7vQOeLzG-DiIbkhwbWUh1QCNHq3X2F4NheZzqRQ/edit",
    "2/9/17",   "https://docs.google.com/spreadsheets/d/1doYWfinNiyCEH4UDIkWcHoypB8qAy1K7u9_VoSPRBSs/edit",
    "1/19/17",  "https://docs.google.com/spreadsheets/d/138WSQpkr1UujaS_rxtrUZMyzhuPLuxpxAUTTMELXAFU/edit",
    "9/8/16",   "https://docs.google.com/spreadsheets/d/1mvA-DM0s9LZPJQFzKH4zbC_vUeQAKghD9HGpo2osW_k/edit",
    "9/29/16",  "https://docs.google.com/spreadsheets/d/1hXrd0kJufsVUHit3D-9DpRaufWo9VYdo3_Vq9xsRZqY/edit",
    "9/22/16",  "https://docs.google.com/spreadsheets/d/1Yyogw-AxbnRLA5EWIdLfqnkb9Fp2UXs5bs6hPsxwkCE/edit",
    "9/15/16",  "https://docs.google.com/spreadsheets/d/1Zh3yIbqbBZuaKRMI-sRmdT0HULsDRtCHP-nfrSoxr38/edit",
    "10/6/16",  "https://docs.google.com/spreadsheets/d/1GRFSMLqc22C9B4VpWr-3h9VF6CjVAa2WgMmwEzPvpEc/edit",
    "10/27/16", "https://docs.google.com/spreadsheets/d/1WyQx3MPVYlLJ0zzMvMFUd8Z-dbsND8SvWywLKNMu_0s/edit",
    "10/20/16", "https://docs.google.com/spreadsheets/d/1I46UirwBOFL_JD2AMdtIyrct3pil2zJetfl5ktxKIuY/edit",
    "10/13/16", "https://docs.google.com/spreadsheets/d/1-4iX3DE1A44XsXMjyfozFNl-yMSUS0gKC_vheHwekC4/edit",
    "11/8/16",  "https://docs.google.com/spreadsheets/d/1K5UqKwHiLZqhU6VgXhomLWsFh6yTmG2pH_RolpFoeU0/edit",
    "11/17/16", "https://docs.google.com/spreadsheets/d/130Mq-SprpRNGUblyqv3BH8K9R1kU5pvSna5jqrKRjxg/edit",
    "11/3/16",  "https://docs.google.com/spreadsheets/d/13JtzjtKcqOyz__dkd4pNDU10Wd9cQg_uHLqt_HIU0ww/edit",
    "5/4/17",   "https://docs.google.com/spreadsheets/d/12RlIV6ZkGRPqnnZIkkDxv_qQbPlRDigob8Sg4Rw9C10/edit",
    "5/11/17",  "https://docs.google.com/spreadsheets/d/1jaM4o0sxVic1JxNMhEL7e_w60v9XHUYlzgkLEiPWPKA/edit",
    "3/9/17",   "https://docs.google.com/spreadsheets/d/1lVGgR9oPUFl8Pp6eHwgZEeI8doXUP8MuZEguqFNgpFo/edit",
    "3/30/17",  "https://docs.google.com/spreadsheets/d/1K4iY69vTE2xikmrLFIRZhQIXdMzUZhvUSSlZzT8vYj0/edit",
    "3/23/17",  "https://docs.google.com/spreadsheets/d/1m85xupzlW5tKKksEFjJrrTgol0lXkPs5YWQa8ObpA5c/edit",
    "3/2/17",   "https://docs.google.com/spreadsheets/d/1jWBR_gTMg-_41jxribNh2LcVN2f7TiyCKSLg0QEgLB8/edit",
    "3/16/17",  "https://docs.google.com/spreadsheets/d/1JmzsjnwlXwvdtYTaBVVh_jOoaJLQSzhewuDBhIU13eI/edit",
    "1/5/17",   "https://docs.google.com/spreadsheets/d/1i1HjVSHX8a2My2Ih74vg3JLL6kUgjbs5LZeRi4BNhWM/edit",
    "1/12/17",  "https://docs.google.com/spreadsheets/d/1TxTQ-hh5CspRkFLMKl9g881rCCNZvnpFhCZvZQH38FE/edit"), 
  ncol = 2, byrow = T)

urlTable = as.data.frame.matrix(x = urlTable, stringsAsFactors = F) # Convert it to a data.frame
colnames(urlTable) = c("Date","URL")                                # Add column names
urlTable$Date2 = as.Date(urlTable$Date, format = "%m/%d/%y")        # Add a date column that has actual date values
urlTable = urlTable[order(urlTable$Date2),]                         # Sort the table by date
rownames(urlTable) = NULL                                           # remove row names


# Get all of the demerit worksheets ####

nonStudents = c(111111111, 222222222, 333333333, 444444444)         # These are ID's that do not actually belong to students

# Create a list containing all of the demerit worksheets
demeritList = vector(mode = "list", length = nrow(urlTable))        # set up a list to hold data.frames of demerits
for(i in 1:length(demeritList)){                                    # This loop is going to take a while
  print(paste0(i, " of ", length(demeritList)))                     # Print the counter
  currentSheet = gs_url(urlTable$URL[i])                            # Get the current demerit spreadsheet
  DemeritTable = gs_read(ss = currentSheet, ws = 1)                 # Load it into a data.frame 
  DemeritTable = DemeritTable[!(DemeritTable$ID %in% nonStudents),] # Remove the ID's of nonstudents
  demeritList[[i]] = DemeritTable                                   # Load that data.frame into the list
  names(demeritList)[i] = as.character(urlTable$Date2[i])           # Name the list element using the date
}

# Check them to make sure they worked
if(!(all(unlist(lapply(X = demeritList, FUN = is.data.frame))))){
  print("There is a problem with one or more of the demerit spreadsheets.")
}

# Create a list containing all of the demerit totals by student by week
demeritTotals = vector(mode = "list", length = nrow(urlTable)) # set up a list to hold demerit totals
for(i in 1:length(demeritList)){
  x = demeritList[[i]]                                               # Get the current demerit spreadsheet
  print(paste0(i, " ", colnames(x)[1]))                              # Print the counter and the name of the first column
  demeritTable = data.frame(                                         # Collapse it to get sums of demerits
    X1 = apply(X = x[,4:ncol(x)], MARGIN = 1, FUN = sum, na.rm = T)) 
  colnames(demeritTable) = names(demeritList)[i]                     # Add a column name using the date
  rownames(demeritTable) = x$ID                                      # Add row names using student ID's
  demeritTotals[[i]] = demeritTable                                  # load it into the demeritTotals list
  names(demeritTotals)[i] = as.character(urlTable$Date2[i])          # Name the list element using the date
}

# For each data.frame in the demeritTotals list, transpose it so that there is one column per student
for(i in 1:length(demeritTotals)){ demeritTotals[[i]] = as.data.frame.matrix(t(demeritTotals[[i]])) }

# Combine the demerit totals into a single data.frame with one column per student and one row per week
AllDemeritTotals = as.data.frame(rbindlist(l = demeritTotals, use.names = T, fill = T, idcol = "Week"))



# Calculate consecutive weeks by student ####

Consecutive = integer(0)                                         # initialize vector of the max consecutive weeks of 0 demerits by student
for(i in 2:ncol(AllDemeritTotals)){                              # for each column after the first one (ie for each student)
  entries = AllDemeritTotals[,i]                                 # Get the entries for that column (# demerits by week for that student)
  anyDemeritsByWeek = as.integer(VbetterComp(entries,0))         # Determine which weeks had 0 demerits
  DemeritString = paste0(anyDemeritsByWeek, collapse = "")       # Collapse it into a string of 1's and 0's
  y = strsplit(x = DemeritString, split = "0", fixed = T)[[1]]   # Get strings of consecutive 1's (consecutive weeks with 0 demerits)
  Consecutive = c(Consecutive,max(nchar(y)))                     # Get the length of the longest run of 0 demerits and append it to Consecutive
} # /for
Consecutive = data.frame(consecWeeks = Consecutive)              # Convert it to a data.frame
Consecutive$ID = colnames(AllDemeritTotals)[-1]                  # Add student ID's


# Summarize the info ####
summary(as.factor(Consecutive$consecWeeks))
summary(Consecutive)
sqrt(var(Consecutive))
ggplot(data = Consecutive, mapping = aes(consecWeeks)) +         # plot the number of students who had each possible length of consecutive demeritless weeks
  geom_histogram(bins = max(Consecutive$consecWeeks) +1, col="red") + 
  labs(y = "Number of Students")


# Generate output ####
Consecutive$LastName =   Workbook$Last.Name[match(Consecutive$ID, Workbook$`Local.ID.(optional)`)]
Consecutive$FirstName =  Workbook$First.Name[match(Consecutive$ID, Workbook$`Local.ID.(optional)`)]
Consecutive$GradeLevel = Workbook$`Grade.(leave.blank.if.no.longer.enrolled)`[match(Consecutive$ID, Workbook$`Local.ID.(optional)`)]
Consecutive = Consecutive[order(Consecutive$consecWeeks, decreasing = T),]
write.csv(x = Consecutive, file = "ConsecutiveWeeksOfNoDemerits.csv", row.names = F)

# Get the total number of demerits per student ####
TotalDemeritsByStudent = apply(X = AllDemeritTotals[2:ncol(AllDemeritTotals)], MARGIN = 2, FUN = sum, na.rm = T) 
summary(TotalDemeritsByStudent)


# Max demerit earner info ####
maxEarner = names(TotalDemeritsByStudent[TotalDemeritsByStudent == max(TotalDemeritsByStudent)]) # ID's of the max demerit earners 
for(i in 1:length(demeritList)){
  x = demeritList[[i]]
  weekname = names(demeritList)[i]
  maxEarned = max(x[x$ID == as.integer(maxEarner),4:ncol(x)],na.rm = T)
  print(paste0(c("Entry: ", i, ",  Week: ", weekname, ",  Demerits: ", maxEarned), collapse = ""))
}


