# Measures of Interim Progress
# This has to do with how SED accountability measures are calculated.

MIPdata = read.xlsx(xlsxFile = file.choose(), sheet = 2)
MIPdata$SUBJECT.NAME
MIPdata[MIPdata$SUBJECT.NAME == "Chronic Absenteeism",3:18] = 100 - MIPdata[MIPdata$SUBJECT.NAME == "Chronic Absenteeism",3:18]
MIPdata$SUBJECT.NAME[MIPdata$SUBJECT.NAME == "Chronic Absenteeism"] = "Not Chronically Absent"

MIPdata2 = vector(mode = "list", length = 7)
names(MIPdata2) = MIPdata$SUBJECT.NAME

for(i in 1:7){
  x = data.frame(Year = c("16-17", "17-18", "18-19", "19-20", "20-21", "21-22"), stringsAsFactors = F)
  x$MeetStateGoal = MIPdata$STATE.LONG.TERM.GOAL[i]
  x$ExceedStateGoal = MIPdata$STATE.EXCEED.LONG.TERM.GOAL[i]
  x$StateMIP = NA_real_
  x$SchoolMIP = NA_real_
  for(j in 2:6){
    x$StateMIP[j] = MIPdata[i,5+j]
    x$SchoolMIP[j] = MIPdata[i,11+j]
  }
  x$LowerMIP = VbetterMin(x$StateMIP, x$SchoolMIP)
  x$HigherMIP = VbetterMax(x$StateMIP, x$SchoolMIP)
  
  x$StateMIP = NULL
  x$SchoolMIP = NULL
  
  x$Performance = NA_real_
  x$Performance[1] = MIPdata[i,12]
  x$Performance[2] = MIPdata[i,18]
  
  MIPdata2[[i]] = x
}


for(i in 1:7){
  x = MIPdata2[[i]]
  x = melt(x, id.vars = "Year")
  x$isPerformance = 1
  x$isPerformance[x$variable == "Performance"] = 2
  x$isPerformance = as.factor(x$isPerformance)
  outgraph = ggplot(data = x, aes(x = Year, y = value, group = variable, color = variable)) + 
    geom_point(aes(size = isPerformance)) + 
    geom_line() +
    labs(title = names(MIPdata2[i]))
  print(outgraph)
}

