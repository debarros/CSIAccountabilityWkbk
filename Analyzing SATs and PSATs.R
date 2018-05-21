#Analyzing SAT's and PSAT's

#-------------------#
#### PSAT Trends ####
#-------------------#

# Make a copy of the PSAT object
PSAT = PSAT.raw
str(PSAT)



#------------------------------------#
#### Predicting SAT based on PSAT ####
#------------------------------------#
SAT$PSAT.Read = NA
SAT$PSAT.Write = NA
SAT$PSAT.Math = NA
SAT$Total = SAT$Reading + SAT$Math + SAT$Writing
SAT$RM = SAT$Reading + SAT$Math

for (i in 1:nrow(SAT)){
  row = which((PSAT$ID == SAT$ID[i]) & (PSAT$Year == SAT$Year[i]-1))
  if(length(row) == 0){}else{ 
    if(!is.na(PSAT$Reading[row])){SAT$PSAT.Read[i] = PSAT$Reading[row]}
    if(!is.na(PSAT$Writing[row])){SAT$PSAT.Write[i] = PSAT$Writing[row]}
    if(!is.na(PSAT$Math[row])){SAT$PSAT.Math[i] = PSAT$Math[row]}
  }
}

SAT$PSAT.Total = SAT$PSAT.Read + SAT$PSAT.Write + SAT$PSAT.Math
SAT$PSAT.RM = SAT$PSAT.Read + SAT$PSAT.Math



# Graphs and analysis based on the new scoring format ####

#Set up data
SAT.new = SAT[SAT$Year > 2015,]
SAT.new$Year = as.factor(SAT.new$Year)
SAT.new = SAT.new[!is.na(SAT.new$PSAT.Read),]
max(SAT.new[,c("Reading","Math","PSAT.Read","PSAT.Math")], na.rm = T)

#make graphs
RM.new = ggplot(SAT.new, aes(x=PSAT.RM, y=RM)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) + # Extend regression lines
  ylim(600,1200) + xlim(600, 1200) + #set limits on axes
  labs(x = "PSAT Reading + Math", y = "SAT Reading + Math", title = "11th Grade 2015 PSAT to 2016 SAT: Total") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash")


Math.new = ggplot(SAT.new, aes(x=PSAT.Math, y=Math)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) + # Extend regression lines
  ylim(250,650) + xlim(250, 650) + #set limits on axes
  labs(x = "PSAT Math", y = "SAT Math", title = "11th Grade 2015 PSAT to 2016 SAT: Math") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash")


Reading.new = ggplot(SAT.new, aes(x=PSAT.Read, y=Reading)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) + # Extend regression lines
  ylim(250,650) + xlim(250, 650) + #set limits on axes
  labs(x = "PSAT Reading", y = "SAT Reading", title = "11th Grade 2015 PSAT to 2016 SAT: Reading") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash")


#make models
RM.new.model1 = lm(RM ~ PSAT.RM, data = SAT.new)
RM.new.model2 = lm(RM ~ PSAT.Read + PSAT.Math, data = SAT.new)
Reading.new.model = lm(Reading ~ PSAT.Read, data = SAT.new)
Math.new.model = lm(Math ~ PSAT.Math, data = SAT.new)

#produce output
RM.new
summary(RM.new.model1)
summary(RM.new.model2)
print(paste0("On average, total score changed by ",mean(SAT.new$RM - SAT.new$PSAT.RM)))
Reading.new
summary(Reading.new.model)
print(paste0("On average, reading score changed by ",mean(SAT.new$Reading - SAT.new$PSAT.Read)))
Math.new
summary(Math.new.model)
print(paste0("On average, math score changed by ",mean(SAT.new$Math - SAT.new$PSAT.Math)))


# year by year comparison
SAT$cYear = as.factor(SAT$Year)
SAT.read.hist = ggplot(data = SAT, mapping = aes(x = Reading)) + 
  geom_histogram(fill = "blue", breaks = seq(200,800, by = 50), col = "red", alpha = 0.5) +
  facet_wrap(~ cYear) + labs(title = "SAT Reading Score Distribution by Year")
SAT.math.hist = ggplot(data = SAT, mapping = aes(x = Math)) + 
  geom_histogram(fill = "blue", breaks = seq(200,800, by = 50), col = "red", alpha = 0.5) +
  facet_wrap(~ cYear) + labs(title = "SAT Math Score Distribution by Year")
SAT.RM.hist = ggplot(data = SAT, mapping = aes(x = RM)) + 
  geom_histogram(fill = "blue", breaks = seq(400,1600, by = 100), col = "red", alpha = 0.5) +
  facet_wrap(~ cYear) + labs(title = "SAT Reading+Math Distribution by Year")

# Graphs and analysis based on the old scoring format ####

SAT$Prep = 0
SAT$Prep[SAT$Year > 2013] = 1
SAT$Prep[which(SAT$Year > 2014)] = 2

SAT$Year = as.factor(SAT$Year)
SAT$Prep = as.factor(SAT$Prep)
SAT.Prep = SAT[which(SAT$Prep != 0),]

p1 = ggplot(SAT, aes(x=PSAT.RM, y=RM, color=Year)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

p2 = ggplot(SAT, aes(x=PSAT.RM, y=RM, color=Prep)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines


p3 = ggplot(SAT.Prep, aes(x=PSAT.RM, y=RM, color=Prep)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) # Extend regression lines

p4 = ggplot(SAT, aes(x=PSAT.Total, y=Total, color=Prep)) + 
  geom_point (aes(shape = SAT$Prep, size = 3)) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) + # Extend regression lines
  theme(text = element_text(size=20))

model1RM = lm(formula = RM ~ PSAT.RM + Year, data = SAT)
model2RM = lm(formula = RM ~ PSAT.RM + Prep, data = SAT)
model1M = lm(formula = Math ~ PSAT.Math + Year, data = SAT)
model2M = lm(formula = Math ~ PSAT.Math + Prep, data = SAT)
model1R = lm(formula = Reading ~ PSAT.Read + Year, data = SAT)
model2R = lm(formula = Reading ~ PSAT.Read + Prep, data = SAT)
model1T = lm(formula = Total ~ PSAT.Total + Year, data = SAT)
model2T = lm(formula = Total ~ PSAT.Total + Prep, data = SAT)
RMbyAll = lm(formula = RM ~ PSAT.Read + PSAT.Write + PSAT.Math + Prep, data = SAT)
RMbyRandM = lm(formula = RM ~ PSAT.Read + PSAT.Math + Prep, data = SAT)
TotalbyAll = lm(formula = Total ~ PSAT.Read + PSAT.Write + PSAT.Math + Prep, data = SAT)
TotalbyRandM = lm(formula = Total ~ PSAT.Read + PSAT.Math + Prep, data = SAT)

summary(model1RM)
summary(model2RM)
summary(model1M)
summary(model2M)
summary(model1R)
summary(model2R)
summary(model1T)
summary(model2T)
summary(RMbyAll)
summary(RMbyRandM)
summary(TotalbyAll)


summary(model1RM)$adj.r.squared
summary(model2RM)$adj.r.squared
summary(model1M)$adj.r.squared
summary(model2M)$adj.r.squared
summary(model1R)$adj.r.squared
summary(model2R)$adj.r.squared
summary(model1T)$adj.r.squared
summary(model2T)$adj.r.squared
summary(RMbyAll)$adj.r.squared
summary(RMbyRandM)$adj.r.squared
summary(TotalbyAll)$adj.r.squared

plot(TotalbyAll)

SAT.Remove = SAT[which(SAT$PSAT.Total < 195),]
TotalbyAll2 = lm(formula = Total ~ PSAT.Read + PSAT.Write + PSAT.Math + Prep, data = SAT.Remove)
summary(TotalbyAll2)

p5 = ggplot(SAT.Remove, aes(x=PSAT.Total, y=Total, color=Prep)) + 
  geom_point (aes(shape = SAT.Remove$Prep, size = 3)) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE,    # Don't add shaded confidence region
              fullrange=TRUE) + # Extend regression lines
  theme(text = element_text(size=20))


#Choose the model with the best adjusted R squared value



#---------------------------------------------------#
#### Computations for the Accountability Dossier ####
#---------------------------------------------------#


# Measure 1 - Each year, the average performance of students in the 10th grade 
# will exceed the state average on the PSAT tests in Critical Reading and Math.
# The subset for this is the set of students who were enrolled in 10th grade 
# as of the date of the PSAT in the relevant year.
PSAT = PSAT.raw
str(PSAT)
PSAT.10 = PSAT[PSAT$Grade == 10,]
PSAT.10.relevant = PSAT.10[PSAT.10$Year == 2016,]
str(PSAT.10.relevant)
PSAT.10.relevant$Math = as.numeric(PSAT.10.relevant$Math)
summary(PSAT.10.relevant)


# Measure 2 - Each year, the average performance of students in the 12th grade will 
# exceed the state average on the SAT or ACT tests in reading and mathematics.
# seniors in the relevant year who had enrolled before the deadline in May of the 
# prior school year to take the June SAT, and remained enrolled until after the 
# first SAT administration in the Fall of the relevant year (usually in October).  
# For example, for the 15-16 report, the subset of students includes those who were 
# in 12th grade during the 15-16 school year, enrolled prior to May 27, 2015, and 
# were still enrolled as of October 3, 2015.

SAT = SAT.raw
str(SAT)

z = read.csv(file.choose(), stringsAsFactors = F)
x = z
colnames(x) = GetNiceColumnNames("STUDENT LITE", templates)[1:ncol(x)]
x = x[x$CURRENTGRADELEVELGRADELEVEL == 12,]
x$enrollDate = Workbook$Date.First.Enrolled.at.GTH[match(x$STUDENTIDSCHOOLDISTRICTSTUDENTID, Workbook$`Local.ID.(optional)`)]
x$exitDate = Workbook$Date.left.GTH[match(x$STUDENTIDSCHOOLDISTRICTSTUDENTID, Workbook$`Local.ID.(optional)`)]
x = x[x$enrollDate < as.Date("2016-05-25"),]
x = x[x$exitDate > as.Date("2016-10-01"),]
SAT.relevant = SAT[SAT$ID %in% x$STUDENTIDSCHOOLDISTRICTSTUDENTID,] 
summary(SAT.relevant)

