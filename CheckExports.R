# CheckExports.R

#Check homeless demographic records and program services ####
View(demographics[demographics$`*HOMELESS(HOMELESS INDICATOR)` == "Y",])


unique(pfacts$V5)

homelessDemo = demographics$`*STUDENT ID(SCHOOL DISTRICT STUDENT ID)`[demographics$`*HOMELESS(HOMELESS INDICATOR)` == "Y"]
homelessPF = pfacts$V4[pfacts$V5 == 5566]

View(demographics[demographics$`*STUDENT ID(SCHOOL DISTRICT STUDENT ID)` %in% homlessStudents,])

un = union(homelessDemo, homelessPF)
int = intersect(homelessDemo, homelessPF)

setdiff(un, int)

