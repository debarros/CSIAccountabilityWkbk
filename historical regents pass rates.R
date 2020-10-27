#Analyzing regents scores

# This script calculates regents exam pass rates for prior years.

TestTerms = paste0(2009:2019, " June") #define the terms you want

regentsScores.GTH = regentsScores[which(regentsScores$Location == "GTH"),] #limit the data set to just GTH

tests = unique(regentsScores.GTH$Exam) #get a list of the exams that have been taken

d2.GTH = regentsScores.GTH #create a copy of the data set
d2.GTH$Score = regentsScores.GTH$Score > 64 #convert the test scores to logical pass/fail

#Set up a matrix with a row for each test and a column for each testing session
A = matrix(NA, length(tests)*length(TestTerms)) 
dim(A) = c(length(tests),length(TestTerms))
rownames(A) = tests
colnames(A) = TestTerms

#Fill the matrix with pass rates
for(i in 1:nrow(A)){
  for (j in 1:ncol(A)){
    set = d2.GTH$Score[which((d2.GTH$Exam == rownames(A)[i]) & (d2.GTH$SessionName == colnames(A)[j]))]
    if(length(set) >0){A[i,j] = mean(set)}
  }
}

#export the pass rates
write.csv(A, file = paste0(OutFolder, "/passrates.csv"))

