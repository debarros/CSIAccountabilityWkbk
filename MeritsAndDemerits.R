# Merits and demerits

urlTable = matrix(data = c(
  "5/24/17", "https://docs.google.com/spreadsheets/d/1H6DnqfFcIhHwaiQ_EX6t-j9hFJAXjVRw_USKdCtUwMg/edit",
  "5/18/17", "https://docs.google.com/spreadsheets/d/1D1SrHoRIVDlLkuXljhgFBR4NPU9ZZYz3RqY0_Y4dOF4/edit",
  "4/6/17", "https://docs.google.com/spreadsheets/d/1_AozER7Vm12ndvkWc2Bppa6E3IqDu2KQvGkZf-2oWIg/edit#gid=0",
  "4/12/17", "https://docs.google.com/spreadsheets/d/124x6kMZa0XXnzHwVIOCGyt3H7uC88T38811XE3QqqrA/edit#gid=0",
  "4/27/17", "https://docs.google.com/spreadsheets/d/1RvH2Ormj82-nxWDg87JYJTJVRKnWNnHTLuMMvcVnh0U/edit#gid=0",
  "12/1/16", "https://docs.google.com/a/greentechhigh.org/spreadsheets/d/1sNSb1jRcFzIX9Bz1mKkod-KSpyuTxhq69m7Y8rbt4tQ/edit?usp=drive_web",
  "12/15/16", "https://docs.google.com/spreadsheets/d/1VG9Bk66y2WmLbF5R3DwAuw5PdHACnxIbz0Twg1IxLhM/edit#gid=0",
  "12/21/16", "https://docs.google.com/spreadsheets/d/1o8YrfiS8aGKqGHOc2HEQxjNczK0bkPbp9w30KkLdPuY/edit",
  "12/8/16", "https://docs.google.com/spreadsheets/d/1nLFlhIe9hYGvsoi9I4XDggqMSnveEg3rHv7q5ITpsJ4/edit#gid=0",
  "2/16/17", "https://docs.google.com/spreadsheets/d/1wzyoxWakFOUZBU_a9bjF6K6KifzWybgyZYAPaiG9Ro4/edit#gid=0",
  "2/2/17", "https://docs.google.com/spreadsheets/d/1Gb_E7vQOeLzG-DiIbkhwbWUh1QCNHq3X2F4NheZzqRQ/edit#gid=0",
  "2/9/17", "https://docs.google.com/spreadsheets/d/1doYWfinNiyCEH4UDIkWcHoypB8qAy1K7u9_VoSPRBSs/edit#gid=0",
  "1/19/17", "https://docs.google.com/spreadsheets/d/138WSQpkr1UujaS_rxtrUZMyzhuPLuxpxAUTTMELXAFU/edit#gid=0",
  "9/8/16", "https://docs.google.com/spreadsheets/d/1mvA-DM0s9LZPJQFzKH4zbC_vUeQAKghD9HGpo2osW_k/edit#gid=0",
  "9/29/16", "https://docs.google.com/spreadsheets/d/1hXrd0kJufsVUHit3D-9DpRaufWo9VYdo3_Vq9xsRZqY/edit#gid=0",
  "9/22/16", "https://docs.google.com/spreadsheets/d/1Yyogw-AxbnRLA5EWIdLfqnkb9Fp2UXs5bs6hPsxwkCE/edit#gid=0",
  "9/15/16", "https://docs.google.com/spreadsheets/d/1Zh3yIbqbBZuaKRMI-sRmdT0HULsDRtCHP-nfrSoxr38/edit#gid=0",
  "10/6/16", "https://docs.google.com/spreadsheets/d/1GRFSMLqc22C9B4VpWr-3h9VF6CjVAa2WgMmwEzPvpEc/edit#gid=0",
  "10/27/16", "https://docs.google.com/spreadsheets/d/1WyQx3MPVYlLJ0zzMvMFUd8Z-dbsND8SvWywLKNMu_0s/edit#gid=0",
  "10/20/16", "https://docs.google.com/spreadsheets/d/1I46UirwBOFL_JD2AMdtIyrct3pil2zJetfl5ktxKIuY/edit#gid=0",
  "10/13/16", "https://docs.google.com/spreadsheets/d/1-4iX3DE1A44XsXMjyfozFNl-yMSUS0gKC_vheHwekC4/edit#gid=0",
  "11/8/16", "https://docs.google.com/spreadsheets/d/1K5UqKwHiLZqhU6VgXhomLWsFh6yTmG2pH_RolpFoeU0/edit#gid=0",
  "11/17/16", "https://docs.google.com/spreadsheets/d/130Mq-SprpRNGUblyqv3BH8K9R1kU5pvSna5jqrKRjxg/edit#gid=0",
  "11/3/16", "https://docs.google.com/spreadsheets/d/13JtzjtKcqOyz__dkd4pNDU10Wd9cQg_uHLqt_HIU0ww/edit#gid=0",
  "5/4/17", "https://docs.google.com/spreadsheets/d/12RlIV6ZkGRPqnnZIkkDxv_qQbPlRDigob8Sg4Rw9C10/edit#gid=0",
  "5/11/17", "https://docs.google.com/spreadsheets/d/1jaM4o0sxVic1JxNMhEL7e_w60v9XHUYlzgkLEiPWPKA/edit#gid=0",
  "3/9/17", "https://docs.google.com/spreadsheets/d/1lVGgR9oPUFl8Pp6eHwgZEeI8doXUP8MuZEguqFNgpFo/edit#gid=0",
  "3/30/17", "https://docs.google.com/spreadsheets/d/1K4iY69vTE2xikmrLFIRZhQIXdMzUZhvUSSlZzT8vYj0/edit#gid=0",
  "3/23/17", "https://docs.google.com/spreadsheets/d/1m85xupzlW5tKKksEFjJrrTgol0lXkPs5YWQa8ObpA5c/edit#gid=0",
  "3/2/17", "https://docs.google.com/spreadsheets/d/1jWBR_gTMg-_41jxribNh2LcVN2f7TiyCKSLg0QEgLB8/edit#gid=0",
  "3/16/17", "https://docs.google.com/spreadsheets/d/1JmzsjnwlXwvdtYTaBVVh_jOoaJLQSzhewuDBhIU13eI/edit#gid=0",
  "1/5/17", "https://docs.google.com/spreadsheets/d/1i1HjVSHX8a2My2Ih74vg3JLL6kUgjbs5LZeRi4BNhWM/edit#gid=0",
  "1/12/17", "https://docs.google.com/spreadsheets/d/1TxTQ-hh5CspRkFLMKl9g881rCCNZvnpFhCZvZQH38FE/edit#gid=0"), ncol = 2, byrow = T)

urlTable = as.data.frame.matrix(x = urlTable, stringsAsFactors = F)
colnames(urlTable) = c("Date","URL")
urlTable$Date2 = as.Date(urlTable$Date, format = "%m/%d/%y")
urlTable = urlTable[order(urlTable$Date2),]
rownames(urlTable) = NULL

demeritList = vector(mode = "list", length = nrow(urlTable))

for(i in 1:length(demeritList)){
  print(i)
  currentSheet = gs_url(urlTable$URL[i])
  DemeritTable = gs_read(ss = currentSheet, ws = 1)
  demeritList[[i]] = DemeritTable
  names(demeritList)[i] = as.character(urlTable$Date2[i])
}

demeritTotals = vector(mode = "list", length = nrow(urlTable))
for(i in 1:length(demeritList)){
  x = demeritList[[i]]
  print(paste0(i, colnames(x)[1]))
  demeritTable = data.frame(X1 = apply(X = x[,4:ncol(x)], MARGIN = 1, FUN = sum, na.rm = T))
  colnames(demeritTable) = names(demeritList)[i]
  rownames(demeritTable) = x$ID
  demeritTotals[[i]] = demeritTable
  names(demeritTotals)[i] = as.character(urlTable$Date2[i])
}


for(i in 1:length(demeritTotals)){
  x = t(demeritTotals[[i]])
  y = as.data.frame.matrix(x)
  demeritTotals[[i]] = y
}

AllDemeritTotals = as.data.frame(rbindlist(l = demeritTotals, use.names = T, fill = T, idcol = "Week"))

x = integer(0)

for(i in 2:ncol(AllDemeritTotals)){
  entries = AllDemeritTotals[,i]
  x = c(x,max(nchar(strsplit(x = paste0(as.integer(VbetterComp(entries,0)), collapse = ""), split = "0", fixed = T)[[1]])))
}

z = apply(X = AllDemeritTotals[2:ncol(AllDemeritTotals)], MARGIN = 2, FUN = sum)

z[VbetterComp(z,0)]

x = data.frame(consecWeeks = x)
ggplot(data = x, mapping = aes(consecWeeks)) + geom_histogram(bins = 34, col="red") + 
  labs(y = "Number of Students")


summary(as.factor(x$consecWeeks))




