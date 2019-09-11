source(file="libanalysis_v2.r")
lib <- importdata("libdata.csv")

#September
sep <- specific_month(lib,2019,"09")
find_double_entry(sep)
sep <- sep[-c(22),]
find_double_entry(sep)
numcheck(sep)
sep[54,6] <- "WEEKEND - 2pm"
sep[54,143] <- NA
findmissing(sep)

#August
aug <- specific_month(lib,2019,"08")
aug <- aug[-c(2:20,128),]
find_double_entry(aug)
aug[70,143] <- "WEEKDAY - 12pm"
aug <- aug[-c(87),]
find_double_entry(aug)
numcheck(aug)
aug[31,63] <- 2
numcheck(aug)
aug[105,4] <- "2019-08-25 17:05:45"
findmissing(aug)

exportxlsx(aug)
