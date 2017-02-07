#Credits.R

library("reshape2")

#Export from PowerSchool the entire StoredGrades table
d1 = read.csv("credits.csv")
VarsToUse = c("Student_Number","TermID","Course_Number","EarnedCrHrs")
d2 = d1[,which(names(d1) %in% VarsToUse)] #limit the data to just the relevant columns



students = as.character(unique(d1$Student_Number))
years = as.character(unique(d1$TermID))
courses = as.character(unique(d1$Course_Number))
StoreCodes = as.character(unique(d1$StoreCode))

d3 = d2[d2$EarnedCrHrs > 0,] #limit the data to just records where students earned credit

#Create a code to uniquely identify by student/course/term
d3$code = paste(d3$Student_Number,"-",d3$Course_Number,"-",d3$TermID) 

d4 = data.frame(Code = unique(d3$code)) #create a data.frame with 1 row for each unique student/course/term
d4$credit = NA #add a column for credits
d4$ID = NA #add a column for student ID

for (i in 1:nrow(d4)){                                      #for each unique student/course/term
  d4$credit[i] = max(d3$EarnedCrHrs[d3$code == d4$Code[i]]) #find the max credits for matching records
  d4$ID[i] = d3$Student_Number[d3$code == d4$Code[i]][1]    #load the student ID
}

d5 = data.frame(ID = unique(d4$ID)) #create a data.frame with 1 row for each student
d5$credits = NA  #add a column for credits
for (i in 1:nrow(d5)){                               #for each student
  d5$credits[i] = sum(d4$credit[d4$ID == d5$ID[i]])  #calculate the total credits earned
}


CreditOutput = Workbookraw[,c("Local.ID..optional.","Cohort.Year..year.1st.entered.9th.")]
CreditOutput = CreditOutput[!is.na(CreditOutput$Local.ID..optional.),]

CreditOutput$credits = NA
for (i in 1:nrow(CreditOutput)){
  if(CreditOutput$Local.ID..optional.[i] %in% d5$ID){
  CreditOutput$credits[i] = d5$credits[d5$ID == CreditOutput$Local.ID..optional.[i]]
  }
}

CreditOutput$credits = as.character(CreditOutput$credits)
CreditOutput$credits[is.na(CreditOutput$credits)] = ""

write.csv(CreditOutput,file="creditOutput.csv")


seniors = d1[d1$Grade_Level == 12,]
juniors = d1[d1$Grade_Level == 11,]




