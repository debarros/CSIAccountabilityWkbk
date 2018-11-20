#Make pretty master schedules

# In PowerSchool, get the master schedule in list view
# Copy the table and paste it into the MasterSchedule tab in PowerSchoolAll.xslx
d1 = read.xlsx(xlsxFile = PSLocation, sheet = "MasterSchedule")
d1$Teacher.Name = trimws(d1$Teacher.Name)
for(i in 1:nrow(d1)){
  space_position  = gregexpr(pattern = " ", text = d1$Teacher.Name[i])
  space_position = space_position[[1]][length(space_position[[1]])]
  d1$Teacher.Name[i] = substr(x = d1$Teacher.Name[i], start = 1, stop = space_position + 1)
}

d1$Course.Name = trimws(d1$Course.Name)
d1$Course.short = FullAlignment$ShortName[match(d1$Course.Name, FullAlignment$Course)]

d1 = d1[d1$Course.Name != "Independent Study",]

# Look for missing info
d1[is.na(d1$Room),]
d1[is.na(d1$Teacher.Name),]
d1[is.na(d1$Course.Name),]
d1[is.na(d1$Course.short),]
d1[is.na(d1$Expression),]


# Fix long room names
d1$Room[d1$Room == "ART ROOM "] = "Art"
d1$Room[d1$Room == "CHAPEL "] = "Chapel"
d1$Room[d1$Room == "GYM "] = "Gym"
d1$Room[d1$Room == "CAFE "] = "Cafe"
d1$Room[d1$Room == "209B "] = "209B"
d1$Room[d1$Room == "209A "] = "209A"
d1$Room[d1$Room == "209C "] = "209C"
unique(d1$Room)

# Created the period column
d1$period = gsub(pattern = "[^[:digit:]]", replacement = "", x = d1$Expression)
d1$period[d1$period == "9"] = "A"

# Split courses that last multiple periods into multiple rows
addons = d1[c(),]
for(i in 1:nrow(d1)){
  if(nchar(d1$period[i]) > 1){
    x = d1[i,]
    y = d1[i,]
    x$period = substr(d1$period[i],1,1)
    y$period = substr(d1$period[i],2,2)
    addons = rbind(addons,x, stringsAsFactors = F)
    addons = rbind(addons,y, stringsAsFactors = F)
  }
}
d1 = d1[nchar(d1$period) == 1,]
d1 = rbind(d1, addons, stringsAsFactors = F)

# Find situations where a teacher has two classes in the same period and merge them
d1$Teach.Per = paste0(d1$Teacher.Name, " - ", d1$period)
doops = d1[duplicated(d1$Teach.Per),]
d1 = d1[!duplicated(d1$Teach.Per),]
rownames(d1) = NULL
rownames(doops) = NULL
for(i in 1:nrow(doops)){
  cur.row = which(d1$Teach.Per == doops$Teach.Per[i])
  classes = paste0(unique(c(d1$Course.short[cur.row],doops$Course.short[i])), collapse = " / ")
  d1$Course.short[cur.row] = classes
}


# Create the table of Teacher x Period
d1$entry = paste0(d1$Course.short, "\n", d1$Room)
d2 = melt(d1[,c("Teacher.Name","period","entry")])
output = dcast(data = d2, formula = Teacher.Name ~ period, value.var = "entry")
output = output[,c("Teacher.Name", "A", as.character(1:8))]
output[is.na(output)] = ""
write.csv(x = output, file = paste0(OutFolder,"mastersched.csv"))




# Look for overloaded rooms
d3 = d1
d3$teacher.short = substr(x = d3$Teacher.Name, start = 1, stop = (nchar(d3$Teacher.Name) - 3))
d3 = d3[d3$Course.short != "Lunch",]
row.names(d3) = NULL
d3$Room.Per = paste0(d3$Room, " - ", d3$period)
doops2 = d3[duplicated(d3$Room.Per),]
write.csv(x = d3[d3$Room.Per %in% doops2$Room.Per,], file = paste0(OutFolder,"overloaded_rooms.csv"))

# Find situations where two classes occur in the same room during the same period and merge them
d3 = d3[!duplicated(d3$Room.Per),]
rownames(d3) = NULL
rownames(doops2) = NULL
for(i in 1:nrow(doops2)){
  cur.row = which(d3$Room.Per == doops2$Room.Per[i])
  teachers = paste0(unique(c(d3$teacher.short[cur.row],doops2$teacher.short[i])), collapse = "/")
  d3$Teacher.Name[cur.row] = teachers
}


# Create the table of Room x Period
output2 = dcast(data = d3, formula = Room ~ period, value.var = "Teacher.Name")
output2 = output2[,c("Room", "A", as.character(1:8))]
output2[is.na(output2)] = ""
write.csv(x = output2, file = paste0(OutFolder,"mastersched2.csv"))




