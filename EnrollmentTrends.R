#Enrollment trends


#----------------------#
#### Clean the data ####
#----------------------#

#create the entry date variable
entry = as.Date(Workbook$Date.First.Enrolled.at.GTH, format = "%m/%d/%Y")
entry = as.Date(Workbook$Date.First.Enrolled.at.GTH, origin = "1899-12-30")
which(is.na(entry))

#Create the exit date variable, and set students who haven't left to exit at the end of the year
exit = as.Date(Workbook$Date.left.GTH, format = "%m/%d/%Y")
exit = as.Date(Workbook$Date.left.GTH, origin = "1899-12-30")
exit[is.na(exit)] = as.Date("6/30/2017", format = "%m/%d/%Y")
betterMax(exit)

#--------------------------------------------------------#
#### Graph Total enrollment overlaying calendar years ####
#--------------------------------------------------------#

#This sets the sequence of dates to be used.
enddate = Sys.Date()
dates = seq.Date(from = min(entry), to = enddate, by = 1)

#This grabs the cohorts
cohort = Workbook$Cohort.Year..year.1st.entered.9th.

#Set up the data frame to hold the dates and the enrollment
Enrollment = data.frame(dates)
Enrollment$count = NA
Enrollment$c2006 = NA
Enrollment$c2007 = NA
Enrollment$c2008 = NA
Enrollment$c2009 = NA
Enrollment$c2010 = NA
Enrollment$c2011 = NA
Enrollment$c2012 = NA
Enrollment$c2013 = NA
Enrollment$c2014 = NA
Enrollment$c2015 = NA
Enrollment$c2016 = NA

#Calculate the number of students enrolled on each day
for (i in 1:nrow(Enrollment)){
  Enrollment$count[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i])
  Enrollment$c2006[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2006)
  Enrollment$c2007[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2007)
  Enrollment$c2008[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2008)
  Enrollment$c2009[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2009)
  Enrollment$c2010[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2010)
  Enrollment$c2011[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2011)
  Enrollment$c2012[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2012)
  Enrollment$c2013[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2013)
  Enrollment$c2014[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2014)
  Enrollment$c2015[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2015)
  Enrollment$c2016[i] = sum(entry <= Enrollment$dates[i] & exit >= Enrollment$dates[i] & cohort == 2016)
}

#In order to categorize by school year, create an offset date to move the school year start to the calendar year start
Enrollment$offsetDate = Enrollment$dates - 181 
Enrollment$year = format(Enrollment$offsetDate, "%Y")  #grab the year
Enrollment$adjustedDate = as.Date(paste0(           #build a new date variable
  format(Enrollment$offsetDate, "%m"),"/",          #grab the month
  format(Enrollment$offsetDate, "%d"),"/","2000"),  #set all years to be 2000
  format = "%m/%d/%Y") + 181                        #format it, and move it back to the correct day/month

#Select the dates to be used for the graph
GraphData = Enrollment[Enrollment$year > 2011,]     #ignore the first few years, when data was poor and the school not at capacity
GraphData = GraphData[GraphData$adjustedDate > as.Date("2000-08-30"),]   #ignore the dates before August 30
GraphData = GraphData[GraphData$adjustedDate < as.Date("2001-06-20"),]   #ignore the dates after June 20

#Make the graph - Daily Total Enrollment Over Each Academic Year
p5 = ggplot(GraphData, aes(x=adjustedDate, y=count, color=year)) + 
  geom_point(size = 1, alpha = .2)  +
  labs(y = "Number of Students", x = "Date", title = "Total Enrollment Over Each Year") + 
  coord_cartesian(ylim = c(328, 365)) +
  scale_x_date(labels = date_format("%b"), date_breaks='1 month') +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method="loess",   # Add linear regression lines
              se=F,    # Don't add shaded confidence region
              fullrange=F,  # Extend regression lines?
              span = .5, #adjust wiggliness
                size = 2) +
  # geom_line() +
  theme(text = element_text(size=30)) 
p5



#--------------------------------------------#
#### Graph enrollment by cohort over time ####
#--------------------------------------------#

# Make enrollment_long
str(Enrollment)
colnames(Enrollment)
yrEnroll = Enrollment[,-c(2,14, 15, 16)] #these numbers will have to be adjusted when a new cohort year is added
str(yrEnroll) #this should show "dates" and one column for each cohort year
enrollment_long = melt(yrEnroll, id = "dates")  
str(enrollment_long)
colnames(enrollment_long)[2] = "Cohort"

enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2006"),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2007"),]

enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2008" & enrollment_long$dates < as.Date("2008-08-30")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2009" & enrollment_long$dates < as.Date("2009-08-30")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2010" & enrollment_long$dates < as.Date("2010-08-30")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2011" & enrollment_long$dates < as.Date("2011-08-30")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2012" & enrollment_long$dates < as.Date("2012-08-30")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2013" & enrollment_long$dates < as.Date("2013-08-30")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2014" & enrollment_long$dates < as.Date("2014-08-30")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2015" & enrollment_long$dates < as.Date("2015-08-30")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2016" & enrollment_long$dates < as.Date("2016-08-30")),]

enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2008" & enrollment_long$dates > as.Date("2012-06-20")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2009" & enrollment_long$dates > as.Date("2013-06-20")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2010" & enrollment_long$dates > as.Date("2014-06-20")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2011" & enrollment_long$dates > as.Date("2015-06-20")),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2012" & enrollment_long$dates > as.Date("2016-06-20")),]

p6 = ggplot(data = enrollment_long, aes(x = dates, y = value, colour = Cohort)) + 
  geom_line(size = 2) +
  geom_vline(xintercept = as.numeric(as.Date("2009-09-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("2010-09-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("2011-09-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("2012-09-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("2013-09-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("2014-09-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("2015-09-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("2016-09-01"))) + 
  labs(x = "Time", y = "Cohort Enrollment", title = "Enrollment Over Time By Cohort") + 
  theme(text = element_text(size=30))
p6


#--------------------------------------------------#
#### Graph 4-year enrollment overlaying cohorts ####
#--------------------------------------------------#

str(enrollment_long)
enrollment_long$cohort = as.numeric(substr(enrollment_long$Cohort, 2, 5))

enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2008"),]
enrollment_long = enrollment_long[!(enrollment_long$Cohort == "c2009"),]


enrollment_long$adjusted_dates = enrollment_long$dates - 365*(enrollment_long$cohort + 1)


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#FAA2F2", "#FF2E25", "#CC79A7")

p7 = ggplot(data = enrollment_long, aes(x = adjusted_dates, y = value, colour = Cohort)) + 
  geom_line(size = 2) +
  labs(y = "Number of Students", x = "Year of HS", title = "Enrollment Over High School Years By Cohort") + 
  scale_colour_manual(values=cbbPalette) +
  theme(text = element_text(size=30))
p7

max(enrollment_long$adjusted_dates)
min(enrollment_long$adjusted_dates)


#-----------------------------------------#
#### Estimate enrollment for next year ####
#-----------------------------------------#

EOY = as.Date("2016-06-30") #June 30th of the last school year that has ended
ThisCohort = format(EOY,"%Y")

#Get the average enrollment over the course of the whole year for each grade level, merging across cohorts
FrAve = mean(enrollment_long$value[enrollment_long$adjusted_dates < as.Date("0001-11-01") & enrollment_long$dates < EOY])
SoAve = mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0002-01-01") & enrollment_long$adjusted_dates < as.Date("0002-11-01") & enrollment_long$dates < EOY])
JuAve = mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0003-01-01") & enrollment_long$adjusted_dates < as.Date("0003-11-01") & enrollment_long$dates < EOY])
SeAve = mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0004-01-01") & enrollment_long$adjusted_dates < as.Date("0004-09-01") & enrollment_long$dates < EOY])

FrAve - SoAve
SoAve - JuAve
JuAve - SeAve

#Get the average enrollment over the first MTHS months of the year for each grade level, merging across cohorts
MTHS = 2
FrAveEarly = mean(enrollment_long$value[enrollment_long$adjusted_dates < as.Date(paste0("0001-",MTHS,"-01")) & enrollment_long$dates < EOY])
SoAveEarly = mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0002-01-01") & enrollment_long$adjusted_dates < as.Date(paste0("0002-",MTHS,"-01")) & enrollment_long$dates < EOY])
JuAveEarly = mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0003-01-01") & enrollment_long$adjusted_dates < as.Date(paste0("0003-",MTHS,"-01")) & enrollment_long$dates < EOY])
SeAveEarly = mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0004-01-01") & enrollment_long$adjusted_dates < as.Date(paste0("0004-",MTHS,"-01")) & enrollment_long$dates < EOY])


#Calculate how many students tend get leave between MTHS months into the year and the beginning of the following year
FrLoss = FrAveEarly - SoAve
SoLoss = SoAveEarly - JuAve
JuLoss = JuAveEarly - SeAve

#Calculate the average enrollment for each cohort in the current school year up to the current point in the year
ThisCohort.int = as.integer(ThisCohort)
FrAveNow = mean(enrollment_long$value[enrollment_long$adjusted_dates < as.Date("0001-11-01") & enrollment_long$cohort == ThisCohort.int])
SoAveNow = mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0002-01-01") & enrollment_long$adjusted_dates < as.Date("0002-11-01") & enrollment_long$cohort == ThisCohort.int - 1])
JuAveNow = mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0003-01-01") & enrollment_long$adjusted_dates < as.Date("0003-11-01") & enrollment_long$cohort == ThisCohort.int - 2])
SeAveNow = mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0004-01-01") & enrollment_long$adjusted_dates < as.Date("0004-09-01") & enrollment_long$cohort == ThisCohort.int - 3])

#Calculate simple predictions for next year's average enrollment over the course of the entire year for each cohort
SoPred = FrAveNow - FrLoss
JuPred = SoAveNow - SoLoss
SePred = JuAveNow - JuLoss


#-----------------------------------------------------------------#
#### Predict average total enrollment for the next school year ####
#-----------------------------------------------------------------#

#Additive model
AddModel = SoPred + JuPred + SePred + FrAve

#Reduction model
ReducModel = FrAveNow*(SoAve/FrAveEarly) + 
  SoAveNow*(JuAve/SoAveEarly) + 
  JuAveNow*(SeAve/JuAveEarly) + FrAve


OptimReducModel = FrAveNow*(SoAve/FrAveEarly) + 
  SoAveNow*(JuAve/SoAveEarly) + 
  JuAveNow*(SeAve/JuAveEarly) + FrAveNow


#---------------------------------------------------------------------------------------#
#### Predictions for the early part of the year (average over the first MTHS months) ####
#---------------------------------------------------------------------------------------#

#Additive model
FallPred1 = FrAveEarly +
  FrAveNow + SoAveEarly - FrAveEarly +
  SoAveNow + JuAveEarly - SoAveEarly +
  JuAveNow + SeAveEarly - JuAveEarly

#Reduction model
FallPred2 = FrAveEarly +
  FrAveNow * (SoAveEarly / FrAveEarly) +
  SoAveNow * (JuAveEarly / SoAveEarly) +
  JuAveNow * (SeAveEarly / JuAveEarly)

#Variation on additive model
FallPred3 = FrAveNow +
  FrAveNow + SoAveEarly - FrAveEarly +
  SoAveNow + JuAveEarly - SoAveEarly +
  JuAveNow + SeAveEarly - JuAveEarly

#Variation of reduction model
FallPred4 = FrAveNow +
  FrAveNow * (SoAveEarly / FrAveEarly) +
  SoAveNow * (JuAveEarly / SoAveEarly) +
  JuAveNow * (SeAveEarly / JuAveEarly)

#Average of predictions
FallPredAve = (FallPred4 + FallPred3 + FallPred2 + FallPred1)/4


#Averaged predictions by grade level
FallFroshPredAve = (FrAveEarly + FrAveNow)/2
FallSophPredAve = ((FrAveNow + SoAveEarly - FrAveEarly) + FrAveNow * (SoAveEarly / FrAveEarly))/2
FallJunPredAve = ((SoAveNow + JuAveEarly - SoAveEarly) + SoAveNow * (JuAveEarly / SoAveEarly))/2
FallSenPredAve = ((JuAveNow + SeAveEarly - JuAveEarly) + (JuAveNow * (SeAveEarly / JuAveEarly)))/2
FallAddedPredAve = sum(FallFroshPredAve, FallSophPredAve, FallJunPredAve, FallSenPredAve)



#--------------------------------------------------------#
#### Historical enrollment averages by cohort by year ####
#--------------------------------------------------------#

ThisCohort.int

for(i in 5:1){
  print(paste0("school year ", ThisCohort.int - i))
  print(mean(enrollment_long$value[enrollment_long$adjusted_dates < as.Date("0001-11-01") & enrollment_long$cohort == ThisCohort.int - i]))
  print(mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0002-01-01") & enrollment_long$adjusted_dates < as.Date("0002-11-01") & enrollment_long$cohort == ThisCohort.int - i - 1]))
  print(mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0003-01-01") & enrollment_long$adjusted_dates < as.Date("0003-11-01") & enrollment_long$cohort == ThisCohort.int - i - 2]))
  print(mean(enrollment_long$value[enrollment_long$adjusted_dates > as.Date("0004-01-01") & enrollment_long$adjusted_dates < as.Date("0004-09-01") & enrollment_long$cohort == ThisCohort.int - i - 3]))
}


