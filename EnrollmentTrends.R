# EnrollmentTrends.R

# This script generates graphs showing the enrollment of the high school over time.
# The graphs usually get used for board reports at the beginning of the school year.

#----------------------#
#### Clean the data ####
#----------------------#

cohortCount = 10 # Set the number of recent cohorts to use for certain graphs

# Remove those who never attended
Workbook = Workbook[!(VbetterComp(Workbook$Discharge.Reason, "never attended")),]
Workbook = Workbook[order(Workbook$`Cohort.Year.(year.1st.entered.9th)`),]
rownames(Workbook) = NULL

# Create the entry date variable
entry = Workbook$Date.First.Enrolled.at.GTH
which(is.na(entry))                          # should be integer(0)

# Create the exit date variable, and set students who haven't left to exit at the end of the year
exit = Workbook$Date.left.GTH
exit[is.na(exit)] = schoolYear("end")
betterMax(exit)                              # should be the end of the current year

# Create some basic stuff to use
EOY = schoolYear("end", y = schoolYear("end") - 400)                        # June 30th of the last school year that has ended
BOY = as.Date(c("0001-01-01", "0002-01-01", "0003-01-01", "0004-01-01"))    # Beginning of the school year in adjusted format
eDates = as.Date(c("0001-11-01", "0002-11-01", "0003-11-01", "0004-09-01")) # End of the school year in adjust format
ThisCohort = format(EOY,"%Y")                                               # Cohort of the newest class
ThisCohort.int = as.integer(ThisCohort)
RecentCohorts = paste0("c",c(ThisCohort, as.integer(ThisCohort)-1))         # This and the prior cohort prepended with "c"
MTHS = 3                                                                    # number of months at the beginning of the year
MTHSdates = as.Date(paste0("000",1:4,"-",MTHS,"-01"))                       # adjusted format dates marking MTHS months into each year


#--------------------------------------------------------#
#### Graph Total enrollment overlaying calendar years ####
#--------------------------------------------------------#

# This sets the sequence of dates to be used.
enddate = Sys.Date()
dates = seq.Date(from = min(entry), to = enddate, by = 1)

# This grabs the cohorts
cohort = Workbook$`Cohort.Year.(year.1st.entered.9th)` 
cohort[is.na(cohort)] = betterMax(cohort)              # assume that students with no cohort have the most recent
cohortSet = unique(cohort)
tail(cohort)                                           # should show the most recent cohort year

# Set up the data frame to hold the dates and the enrollment
Enrollment = data.frame(dates)
Enrollment[,c("count",paste0("c",cohortSet))] = NA

# Calculate the number of students enrolled on each day
# Is there a way to do this without loops?
for (i in 1:nrow(Enrollment)){
  Enrollment$count[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i])
  for(j in cohortSet){
    Enrollment[i,paste0("c",j)] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == j)  
  } # /for each cohort
} # /for each date

# Print the BEDS Day enrollment
Enrollment[Enrollment$dates == BedsDate(),]

# In order to categorize by school year, create an offset date to move the school year start to the calendar year start
Enrollment$offsetDate = Enrollment$dates - 181 
Enrollment$year = format(Enrollment$offsetDate, "%Y") # grab the year
Enrollment$adjustedDate = as.Date(paste0(             # build a new date variable
  format(Enrollment$offsetDate, "%m"),"/",            # grab the month
  format(Enrollment$offsetDate, "%d"),"/","2000"),    # set all years to be 2000
  format = "%m/%d/%Y") + 181                          # format it, and move it back to the correct day/month

# Select the dates to be used for the graph
GraphData = Enrollment[Enrollment$year > 2011,]                          # ignore early years (poor data, school not at capacity)
GraphData = GraphData[GraphData$adjustedDate > as.Date("2000-08-30"),]   # ignore the dates before August 30
GraphData = GraphData[GraphData$adjustedDate < as.Date("2001-06-20"),]   # ignore the dates after June 20

# Make the graph - Daily Total Enrollment Over Each Academic Year
p5 = ggplot(GraphData, aes(x=adjustedDate, y=count, color=year)) + 
  geom_point(size = 1, alpha = .2)  +
  labs(y = "Number of Students", x = "Date", 
       title = "Total Enrollment Over Each Year") + 
  coord_cartesian(ylim = c(290, 380)) +
  scale_x_date(labels = date_format("%b"), 
               date_breaks='1 month') +
  scale_colour_hue(l = 50) +                          # Use a slightly darker palette than normal
  geom_smooth(method="loess",                         # Add linear regression lines
              se = F,                                 # Don't add shaded confidence region
              fullrange = F,                          # Extend regression lines?
              span = .4,                              # adjust wiggliness
              size = 2) +
  theme(text = element_text(size=30)) 
p5


#--------------------------------------------#
#### Graph enrollment by cohort over time ####
#--------------------------------------------#

# Make enr_lng, a long version of the enrollment data
yrEnroll = Enrollment[,c("dates",paste0("c",cohortSet))] 
enr_lng = melt(yrEnroll, id = "dates")  
colnames(enr_lng)[2] = "Cohort"

# Remove early cohorts
enr_lng = enr_lng[!(enr_lng$Cohort %in% c("c2006","c2007", "c2008", "c2009")),]

# For each cohort, remove entries prior to August 30th of that year (because that data is suspect)
for(i in cohortSet){
  enr_lng = enr_lng[!(enr_lng$Cohort == paste0("c",i) & enr_lng$dates < as.Date(paste0(i,"-08-30"))),]
}

# For each cohort, remove entries after to June 20th of that cohort's 4th school year (because that data is irrelevant)
for(i in cohortSet){
  enr_lng = enr_lng[!(enr_lng$Cohort == paste0("c",i) & enr_lng$dates > as.Date(paste0(i+4,"-06-20"))),]
}

# Make the graph
p6 = ggplot(data = enr_lng, aes(x = dates, y = value, colour = Cohort)) + geom_line(size = 2)
for(i in 2009:ThisCohort.int){ 
  p6 = p6 + geom_vline(xintercept = as.numeric(as.Date(paste0(i, "-09-01")))) 
}
p6 = p6 + labs(x = "Time", y = "Cohort Enrollment", title = "Enrollment Over Time By Cohort")
p6 = p6 + theme(text = element_text(size=30))
p6


#--------------------------------------------------#
#### Graph 4-year enrollment overlaying cohorts ####
#--------------------------------------------------#

# Convert the cohort variable to numeric
enr_lng$cohort = as.numeric(substr(enr_lng$Cohort, 2, 5))

#remove the early cohorts, since they were quite different
enr_lng = enr_lng[!(enr_lng$Cohort == "c2008"),]
enr_lng = enr_lng[!(enr_lng$Cohort == "c2009"),]

#Change the dates to be the beginning of HS years numbered starting from 1
enr_lng$adj_dates = enr_lng$dates - 365*(enr_lng$cohort + 1)


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#FAA2F2", "#FF2E25", "#CC79A7","#00FF00", "#0000FF", "#00FFFF")
cbbPalette = cbbPalette[1:cohortCount]

cohortsToShow = tail(cohortSet, cohortCount)
enr_lng2 = enr_lng[enr_lng$cohort %in% cohortsToShow,]
p7 = ggplot(data = enr_lng2, aes(x = adj_dates, y = value, colour = Cohort)) + 
  geom_line(size = 2) +
  labs(y = "Number of Students", x = "Year of HS", title = "Enrollment Over High School Years By Cohort") + 
  scale_colour_manual(values=cbbPalette) +
  theme(text = element_text(size=30))
p7

# Create indicators showing whether the cohort is in each current grade
enr_lng2$current9  = (enr_lng2$cohort == ThisCohort.int - 0) + 1
enr_lng2$current10 = (enr_lng2$cohort == ThisCohort.int - 1) + 1
enr_lng2$current11 = (enr_lng2$cohort == ThisCohort.int - 2) + 1
enr_lng2$current12 = (enr_lng2$cohort == ThisCohort.int - 3) + 1


#--------------------------------------------------#
#### Graph 1-year enrollment overlaying cohorts ####
#--------------------------------------------------#

p7.y1 = ggplot(data = enr_lng2, aes(x = adj_dates + 240, y = value, colour = Cohort, size = current9)) + 
  geom_line() +
  labs(y = "Number of Students", x = "Point in the Year", title = "Enrollment During 1st year of Cohort") + 
  theme(text = element_text(size=30), legend.position = "none") +
  scale_x_date(limits = c(as.Date("0001-09-01"), as.Date("0002-06-15")), date_breaks = "1 month", date_labels = "%b")
p7.y1

p7.y2 = ggplot(data = enr_lng2, aes(x = adj_dates + 240, y = value, colour = Cohort, size = current10)) + 
  geom_line() +
  labs(y = "Number of Students", x = "Point in the Year", title = "Enrollment During 2nd year of Cohort") + 
  theme(text = element_text(size=30), legend.position = "none") +
  scale_x_date(limits = c(as.Date("0002-09-01"), as.Date("0003-06-15")), date_breaks = "1 month", date_labels = "%b")
p7.y2

p7.y3 = ggplot(data = enr_lng2, aes(x = adj_dates + 240, y = value, colour = Cohort, size = current11)) + 
  geom_line() +
  labs(y = "Number of Students", x = "Point in the Year", title = "Enrollment During 3rd year of Cohort") + 
  theme(text = element_text(size=30), legend.position = "none") +
  scale_x_date(limits = c(as.Date("0003-09-01"), as.Date("0004-06-15")), date_breaks = "1 month", date_labels = "%b")
p7.y3

p7.y4 = ggplot(data = enr_lng2, aes(x = adj_dates + 240, y = value, colour = Cohort, size = current12)) + 
  geom_line() +
  labs(y = "Number of Students", x = "Point in the Year", title = "Enrollment During 4th year of Cohort") + 
  theme(text = element_text(size=30), legend.position = "none") +
  scale_x_date(limits = c(as.Date("0004-09-01"), as.Date("0005-06-15")), date_breaks = "1 month", date_labels = "%b")
p7.y4


#--------------------------------------------------#
#### Estimate enrollment by grade for next year ####
#--------------------------------------------------#

#Get the average enrollment over the course of the whole year for each grade level, merging across cohorts
wholeYearByGrade = list()
wholeYearByGrade$Fr = mean(enr_lng$value[enr_lng$adj_dates < eDates[1] & enr_lng$dates < EOY])
wholeYearByGrade$FrRecent = mean(enr_lng$value[enr_lng$adj_dates < eDates[1] & enr_lng$dates < EOY & enr_lng$Cohort %in% RecentCohorts])
wholeYearByGrade$So = mean(enr_lng$value[enr_lng$adj_dates > BOY[2] & enr_lng$adj_dates < eDates[2] & enr_lng$dates < EOY])
wholeYearByGrade$Ju = mean(enr_lng$value[enr_lng$adj_dates > BOY[3] & enr_lng$adj_dates < eDates[3] & enr_lng$dates < EOY])
wholeYearByGrade$Se = mean(enr_lng$value[enr_lng$adj_dates > BOY[4] & enr_lng$adj_dates < eDates[4] & enr_lng$dates < EOY])



#Get the average enrollment over the first MTHS months of the year for each grade level, merging across cohorts
earlyByGrade = list()
earlyByGrade$Fr = mean(enr_lng$value[enr_lng$adj_dates < MTHSdates[1] & enr_lng$dates < EOY])
earlyByGrade$FrRecent = mean(enr_lng$value[enr_lng$adj_dates < MTHSdates[1] & enr_lng$dates < EOY & enr_lng$Cohort %in% RecentCohorts])
earlyByGrade$So = mean(enr_lng$value[enr_lng$adj_dates > BOY[2] & enr_lng$adj_dates < MTHSdates[2] & enr_lng$dates < EOY])
earlyByGrade$Ju = mean(enr_lng$value[enr_lng$adj_dates > BOY[3] & enr_lng$adj_dates < MTHSdates[3] & enr_lng$dates < EOY])
earlyByGrade$Se = mean(enr_lng$value[enr_lng$adj_dates > BOY[4] & enr_lng$adj_dates < MTHSdates[4] & enr_lng$dates < EOY])

#Calculate how many students tend to leave between MTHS months into the year and the beginning of the following year
earlyByGrade$FrLoss = earlyByGrade$Fr - wholeYearByGrade$So
earlyByGrade$SoLoss = earlyByGrade$So - wholeYearByGrade$Ju
earlyByGrade$JuLoss = earlyByGrade$Ju - wholeYearByGrade$Se

#Calculate the average enrollment for each cohort in the current school year up to the current point in the year
nowByGrade = list()
nowByGrade$Fr = mean(enr_lng$value[enr_lng$adj_dates < eDates[1] & enr_lng$cohort == ThisCohort.int])
nowByGrade$So = mean(enr_lng$value[enr_lng$adj_dates > BOY[2] & enr_lng$adj_dates < eDates[2] & enr_lng$cohort == ThisCohort.int - 1])
nowByGrade$Ju = mean(enr_lng$value[enr_lng$adj_dates > BOY[3] & enr_lng$adj_dates < eDates[3] & enr_lng$cohort == ThisCohort.int - 2])
nowByGrade$Se = mean(enr_lng$value[enr_lng$adj_dates > BOY[4] & enr_lng$adj_dates < eDates[4] & enr_lng$cohort == ThisCohort.int - 3])

#Calculate simple predictions for next year's average enrollment over the course of the entire year for each cohort
simpleByGrade = list()
simpleByGrade$Fr = wholeYearByGrade$Fr
simpleByGrade$So = nowByGrade$Fr - earlyByGrade$FrLoss
simpleByGrade$Ju = nowByGrade$So - earlyByGrade$SoLoss
simpleByGrade$Se = nowByGrade$Ju - earlyByGrade$JuLoss


#-----------------------------------------------------------------#
#### Predict average total enrollment for the next school year ####
#-----------------------------------------------------------------#

wholeYearPred = list()

#Additive model
wholeYearPred$AddModel.FrAveYear = simpleByGrade$So + simpleByGrade$Ju + simpleByGrade$Se + wholeYearByGrade$Fr
wholeYearPred$AddModel.FrAveNow = simpleByGrade$So + simpleByGrade$Ju + simpleByGrade$Se + nowByGrade$Fr
wholeYearPred$AddModel.FrAveYearRecent = simpleByGrade$So + simpleByGrade$Ju + simpleByGrade$Se + wholeYearByGrade$FrRecent


#Reduction model
wholeYearPred$ReducModel.FrAveYear = nowByGrade$Fr*(wholeYearByGrade$So/earlyByGrade$Fr) + 
  nowByGrade$So*(wholeYearByGrade$Ju/earlyByGrade$So) + 
  nowByGrade$Ju*(wholeYearByGrade$Se/earlyByGrade$Ju) + wholeYearByGrade$Fr
wholeYearPred$ReducModel.FrAveNow = nowByGrade$Fr*(wholeYearByGrade$So/earlyByGrade$Fr) + 
  nowByGrade$So*(wholeYearByGrade$Ju/earlyByGrade$So) + 
  nowByGrade$Ju*(wholeYearByGrade$Se/earlyByGrade$Ju) + nowByGrade$Fr
wholeYearPred$ReducModel.FrAveYearRecent = nowByGrade$Fr*(wholeYearByGrade$So/earlyByGrade$Fr) + 
  nowByGrade$So*(wholeYearByGrade$Ju/earlyByGrade$So) + 
  nowByGrade$Ju*(wholeYearByGrade$Se/earlyByGrade$Ju) + wholeYearByGrade$FrRecent

#Average of predictions
wholeYearPred$Average = mean(unlist(wholeYearPred))

#---------------------------------------------------------------------------------------#
#### Predictions for the early part of the year (average over the first MTHS months) ####
#---------------------------------------------------------------------------------------#

FallPred = list()

#Additive model
FallPred$Add1 = earlyByGrade$Fr +
  nowByGrade$Fr + earlyByGrade$So - earlyByGrade$Fr +
  nowByGrade$So + earlyByGrade$Ju - earlyByGrade$So +
  nowByGrade$Ju + earlyByGrade$Se - earlyByGrade$Ju

#Reduction model
FallPred$Red1 = earlyByGrade$Fr +
  nowByGrade$Fr * (earlyByGrade$So / earlyByGrade$Fr) +
  nowByGrade$So * (earlyByGrade$Ju / earlyByGrade$So) +
  nowByGrade$Ju * (earlyByGrade$Se / earlyByGrade$Ju)

#Variation on additive model
FallPred$Add2 = nowByGrade$Fr +
  nowByGrade$Fr + earlyByGrade$So - earlyByGrade$Fr +
  nowByGrade$So + earlyByGrade$Ju - earlyByGrade$So +
  nowByGrade$Ju + earlyByGrade$Se - earlyByGrade$Ju

#Variation of reduction model
FallPred$Red2 = nowByGrade$Fr +
  nowByGrade$Fr * (earlyByGrade$So / earlyByGrade$Fr) +
  nowByGrade$So * (earlyByGrade$Ju / earlyByGrade$So) +
  nowByGrade$Ju * (earlyByGrade$Se / earlyByGrade$Ju)

#Average of predictions
FallPred$AveragedTotal = mean(unlist(FallPred))


#Averaged predictions by grade level
FallPred$Fr = (earlyByGrade$Fr + nowByGrade$Fr)/2
FallPred$So = ((nowByGrade$Fr + earlyByGrade$So - earlyByGrade$Fr) + nowByGrade$Fr * (earlyByGrade$So / earlyByGrade$Fr))/2
FallPred$Ju = ((nowByGrade$So + earlyByGrade$Ju - earlyByGrade$So) + nowByGrade$So * (earlyByGrade$Ju / earlyByGrade$So))/2
FallPred$Se = ((nowByGrade$Ju + earlyByGrade$Se - earlyByGrade$Ju) + (nowByGrade$Ju * (earlyByGrade$Se / earlyByGrade$Ju)))/2
FallPred$SumOfGradeAverages = sum(FallPred$Fr, FallPred$So, FallPred$Ju, FallPred$Se)


#-------------------------#
#### Print Predictions ####
#-------------------------#


for(i in 0:length(simpleByGrade)){
  if(i == 0) {
    print("Simple Predictions")
  } else {
    print(paste0(names(simpleByGrade)[i]," - ",round(simpleByGrade[[i]], digits = 0)))    
  }
}


for(i in 0:length(wholeYearPred)){
  if(i == 0) {
    print("Predictions for Yearlong Average Enrollment")
  } else {
    print(paste0(names(wholeYearPred)[i]," - ",round(wholeYearPred[[i]], digits = 0)))
  }
}

for(i in 0:length(FallPred)){
  if(i == 0) {
    print("Predictions for Fall Enrollment")
  } else {
    print(paste0(names(FallPred)[i]," - ",round(FallPred[[i]], digits = 0)))
  }
}



#--------------------------------------------------------#
#### Historical enrollment averages by cohort by year ####
#--------------------------------------------------------#

for(i in 5:0){
  tcadj = ThisCohort.int - i             # This Cohort, adjusted
  print(paste0("school year ", tcadj))
  for(j in 1:4){
    thisCount = mean(enr_lng$value[enr_lng$cohort == tcadj - j + 1 & enr_lng$adj_dates < eDates[j] & enr_lng$adj_dates > BOY[j]])
    thisCount = round(thisCount, 1)
    gradelevel = c("Fresh: ", "Soph:  ", "Jun:   ", "Sen:   ")[j]
    print(paste0(gradelevel, thisCount))
  }
  print(" ")
}




#----------------------------------------------#
#### Historical attrition by cohort by year ####
#----------------------------------------------#

# How many students left between the end of one year and MTHS months into the next?
datestouse1 = as.Date(paste0("000",1:3,"-10-15"))  # This corresponds to June 15
datestouse2 = MTHSdates[2:4]
CohortsToUse = unique(enr_lng$cohort)
Drops = c("FrSo", "SoJu", "JuSe")                  # This indicates the split from the end of one year to MTHS into the next
dropMatrix = matrix(data = NA, nrow = length(CohortsToUse), ncol = length(Drops))
rownames(dropMatrix) = CohortsToUse
colnames(dropMatrix) = Drops
dropRateMatrix = dropMatrix

for(i in 1:length(CohortsToUse)){
  currentCohort = CohortsToUse[i]
  for(j in 1:length(Drops)){
    currentDrop = Drops[j]
    startValue = enr_lng$value[enr_lng$cohort == currentCohort & enr_lng$adj_dates == datestouse1[j]]
    endValue   = enr_lng$value[enr_lng$cohort == currentCohort & enr_lng$adj_dates == datestouse2[j]]
    if(length(c(startValue, endValue)) > 1){
      dropMatrix[i,j] = startValue - endValue  
      dropRateMatrix[i,j] = (startValue - endValue)/startValue
    }
  }
}

colMeans(dropMatrix, na.rm = T)

round(colMeans(dropRateMatrix, na.rm = T) * 100,0)
round(dropRateMatrix * 100, 0)

paste0(dropRateMatrix, "%")


