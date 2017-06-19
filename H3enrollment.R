# What does this do?  Something to do with Lit 11 and US History

library(openxlsx)

ccTable = read.xlsx(xlsxFile = "ccTable1415.xlsx", sheet = 2)

str(ccTable)

H3 = ccTable[ccTable$`[02]course_name` %in% c("US History Honors-1 credit", "US History 1 credit", "Literature 11- 1 credit", "Literature 11 Honors"),]

unique(H3$SectionID)
H3$SectionID = abs(H3$SectionID)
H3$SectionCode = paste0("s",H3$SectionID)
sectionCodes = unique(H3$SectionCode)
min(H3$DateEnrolled, H3$DateLeft)
max(H3$DateEnrolled, H3$DateLeft)

sectionCounts = data.frame(date = min(H3$DateEnrolled, H3$DateLeft):max(H3$DateEnrolled, H3$DateLeft))

sectionCounts[,sectionCodes] = NA_integer_

i = 100
j = sectionCodes[1]


for(i in 1:nrow(sectionCounts)){
  for(j in sectionCodes){
    sectionCounts[i,j] = sum(H3$SectionCode == j & H3$DateEnrolled <= sectionCounts$date[i] & H3$DateLeft >  sectionCounts$date[i])
  }
}

apply(X = sectionCounts, MARGIN = 2, FUN = max)

sectionCounts = sectionCounts[-c(1:5, nrow(sectionCounts):(nrow(sectionCounts)-5)),]

summary(sectionCounts)

H3[H3$SectionCode %in% c("s3379", "s3490"),]
H3[H3$SectionCode %in% c("s3357", "s3489"),]


