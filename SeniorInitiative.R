#SeniorInitiative.R

#This is a largely undocumented scrip to find students who should participate in Senior Initiative based on their regents scores
#The regents scores in the workbook must be updated before this script is run

names(Workbookraw)

Workbook = Workbookraw[!is.na(Workbookraw$Local.ID..optional.),]

Workbook = Workbook[Workbook$Still.Enrolled. == "yes",]

Workbook = Workbook[Workbook$Grade..leave.blank.if.no.longer.enrolled. > 11,]

x1 = which(!(Workbook$GlobalScaled.Score > 64))
x2 = which(is.na(Workbook$GlobalScaled.Score))

Workbook2 = Workbook[c(x1,x2),]

Workbook3 = Workbook2[,c(2:5,135,3*(12:26)+1), ]

Workbook3[which(is.na(Workbook3), arr.ind = TRUE)] = 0

Workbook3$Bio55 = Workbook3$BioScaled.Score > 54
Workbook3$Bio65 = Workbook3$BioScaled.Score > 64
Workbook3$Ela55 = Workbook3$EnglishScaled.Score > 54 | Workbook3$Common.Core.ELAScaled.Score > 54 
Workbook3$Ela65 = Workbook3$EnglishScaled.Score > 64 | Workbook3$Common.Core.ELAScaled.Score > 64 
Workbook3$Sci55 = Workbook3$Earth.ScienceScaled.Score > 54 | Workbook3$ChemistryScaled.Score > 54 | Workbook3$PhysicsScaled.Score > 54
Workbook3$Sci65 = Workbook3$Earth.ScienceScaled.Score > 64 | Workbook3$ChemistryScaled.Score > 64 | Workbook3$PhysicsScaled.Score > 64
Workbook3$Alg = VbetterMax(Workbook3$AlgebraScaled.Score, Workbook3$Common.Core.AlgebraScaled.Score)
Workbook3$Geom = VbetterMax(Workbook3$GeometryScaled.Score, Workbook3$Scaled.Score)
Workbook3$Maths55 = (Workbook3$Alg>54) + (Workbook3$Geom>54) + (Workbook3$TrigScaled.Score>54)
Workbook3$Maths65 = (Workbook3$Alg>64) + (Workbook3$Geom>64) + (Workbook3$TrigScaled.Score>64)
Workbook3$US55 = Workbook3$US.HistoryScaled.Score > 54
Workbook3$US65 = Workbook3$US.HistoryScaled.Score > 64
Workbook3$Global55 = Workbook3$GlobalScaled.Score > 54
Workbook3$Global65 = Workbook3$GlobalScaled.Score > 64

Workbook3$safetyNet = FALSE
Workbook3$safetyNet[Workbook3$IEP == "Yes"] = TRUE
Workbook3$safetyNet[Workbook3$Local.ID..optional. == 141510046] = TRUE

Workbook3$Bio = (Workbook3$Bio55 & Workbook3$safetyNet) | Workbook3$Bio65
Workbook3$Ela = (Workbook3$Ela55 & Workbook3$safetyNet) | Workbook3$Ela65
Workbook3$Sci = (Workbook3$Sci55 & Workbook3$safetyNet) | Workbook3$Sci65
Workbook3$Maths = Workbook3$Maths65
Workbook3$Maths[Workbook3$safetyNet] = Workbook3$Maths55[Workbook3$safetyNet]
Workbook3$US = (Workbook3$US55 & Workbook3$safetyNet) | Workbook3$US65
Workbook3$Global = (Workbook3$Global55 & Workbook3$safetyNet) | Workbook3$Global65

Workbook3 = Workbook3[!Workbook3$Global,]

Workbook3$TotalExams = 0

Workbook3$TotalExams = Workbook3$Bio + Workbook3$Ela + Workbook3$Maths + Workbook3$US + Workbook3$Global + Workbook3$Sci
Workbook3$FourPlusOne = Workbook3$Bio & Workbook3$Ela & (Workbook3$Maths > 0) & (Workbook3$US | Workbook3$Global) & (Workbook3$TotalExams > 4)
Workbook3$UHR = FALSE
Workbook3$UHR[Workbook3$Local.ID..optional. %in% c(201277388, 1213178, 201219916, 201255325, 201284565, 201275705,131412095, 1213130, 201231724, 201252810, 201133231, 201250038)] = TRUE
Workbook3$UHR.Enough = Workbook3$UHR & ((Workbook3$Maths > 1) | Workbook3$Sci)




enroll = read.csv("enrollments.csv")


GlobStuds = unique(enroll$X.01.Student_Number[enroll$X.02.course_name %in% c("Global History II 1-credit", "Global History II Honors- 1 credit")])

Workbook3$InGlobal = Workbook3$Local.ID..optional. %in% GlobStuds

Workbook3$InBuilding = Workbook3$Local.ID..optional. %in% unique(enroll$X.01.Student_Number)



output = Workbook3[(Workbook3$InBuilding &! Workbook3$InGlobal),c(1:3,36:42,44)]
names(output)

sum((Workbook3$InBuilding &! Workbook3$InGlobal))
sum((! Workbook3$InGlobal))
sum((Workbook3$InBuilding))

output$Cohort = NA
i = 5
for (i in 1:nrow(output)){
  output$Cohort[i]= Workbook2$Cohort.Year..year.1st.entered.9th.[which(Workbook2$Local.ID..optional. == output$Local.ID..optional.[i])]
}

warnings()
write.csv(output,file = "seniorInitiative.csv")


str(enroll)
x = enroll[enroll$X.01.Student_Number %in% c(   ),] #<list of student numbers here>#

x = x[order(x$X.01.Student_Number),]
lost.df = lost.df[order(lost.df$Student,lost.df$Percent),]

