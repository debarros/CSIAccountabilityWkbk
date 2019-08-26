# Run Different Schedules.R

# This script is intended to try all possible schedules of summer school classes and evaluate each one.
# The part where it creates all possible combinations of all possible permutations of several sets should be a function (or maybe a package)

library(arrangements)

p1 = ipermutations(3,1)$collect()
p2 = ipermutations(3,2)$collect()
p3 = ipermutations(3,3)$collect()

p1 = p1 + 6
p2 = p2 + 3

p1
p2
p3

x = vector(mode = "list", length = teach)

expandlist = list(1:nrow(p1), 1:nrow(p2), 1:nrow(p3))

selector = expand.grid(expandlist)
str(selector)
selector$Code = selector$Var1 + 10*selector$Var2 + 100*selector$Var3
length(unique(selector$Code))
nrow(selector)

p4 = p3

p4 = p4 + 100

teachers = unique(sectionTable$Teacher)

teachCount = integer(0)

for(i in teachers){  teachCount = c(teachCount, sum(sectionTable$Teacher == i)) }

npermutations(3,)

teachPerms = integer(0)
for(i in teachCount){  teachPerms = c(teachPerms, npermutations(3,i)) }

teachArr = 1
for(i in teachPerms){  teachArr = teachArr * i }

teachlist = vector(mode = "list", length = length(teachers))
names(teachlist) = teachers

for(i in 1:length(teachlist)){
  curTeach = teachers[i]
  sectionCount = sum(curTeach == sectionTable$Teacher)
  teachlist[[i]] = ipermutations(3,sectionCount)$collect()
}


teacherPermSequence = vector(mode = "list", length = length(teachers))
for(i in 1:length(teacherPermSequence)){
  teacherPermSequence[[i]] = 1:nrow(teachlist[[i]])
}
selector = expand.grid(teacherPermSequence)

nrow(selector) == teachArr

i = 50

j = 3

x = matrix(data = 0, nrow = nrow(sectionTable), ncol = nrow(selector)) # Initialize the matrix of possible schedules
for(i in 1:nrow(selector)){                    # for each combination of orderings
  choices = as.integer(selector[i,])          # grab the vector indicating which ordering to use for each teacher
  thisColumn = integer(0)                      # create an empty vector to hold the current schedule
  for(j in 1:length(choices)){                 # for each teacher
    thisTable = teachlist[[j]]                 # Get the table of orderings for that teacher
    thisOrdering = thisTable[choices[j],]      # Get the desired ordering for that teacher by using the selection vector
    thisColumn = c(thisColumn, thisOrdering)   # Append that teacher's ordering to the current schedule
  }
  x[,i] = thisColumn                           # Load that schedule into the matrix of possible schedules 
}

str(x)
str(thisColumn)
thisTable[1,]
str(choices[])
str(matrixbuilder)
dim(x)
str(newMatrix)
newMatrix[,1:20]
x[relevantRows,1:20]
length(x[relevantRows,])
length(newMatrix)
str(x[relevantRows,])

identical(newMatrix[,1:20], x[relevantRows,1:20])
all(newMatrix[,] == x[relevantRows,])

results = matrix(0,2,teachArr)
rownames(results) = c("Manual", "Split")
str(x)
x[1:18,1:20]
results[1:2,1:20]


for(i in 1:teachArr){
  print(i)
  sectionTable1 = sectionTable
  sectionTable1$Period = x[,i]
  
  sectionTable1 = sectionTable1[order(sectionTable1$Period == 2, sectionTable1$Period == 1, decreasing = T),]
  rownames(sectionTable1) = NULL
  sectionTable1$Preference = 1:nrow(sectionTable1)
  
  AssignmentList = list("SummerEnrollments" = SummerEnrollments, "sectionTable" = sectionTable1, "students" = students)
  AssignmentList = Summer.AssignStudents(AssignmentList)
  
  results[1,i] = sum(AssignmentList$SummerEnrollments$ManualAdjustment)
  results[2,i] = sum(AssignmentList$students$HasP1 & AssignmentList$students$HasP3 & !AssignmentList$students$HasP2)
  
  if(sum(results[,i]) == 0){
    stop(paste0("use column ", i))
  }
}

i = 1
arrangementCode = integer(0)
for(i in 1:ncol(x)){
  arrangementCode = c(arrangementCode, sum((x[,i] - 1) * (3 ^ (0:17))) )
}

length(arrangementCode)
length(unique(arrangementCode))
length(unique(arrangementCode[1:384]))

summary(t(results[,1:10000]))
summary(as.factor(results[1,1:10000]))
summary(as.factor(results[2,1:10000]))


str(results)
summary(as.factor(results[2,]))



384/2
192/2
96/12




Large = matrix(1:35,5,7)
entries = 51:64
entryMatrix = matrix(entries, nrow = 2)
Large[2:3,] = entries





