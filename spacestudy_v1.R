source(file="libanalysis_v1.r")
lib <- importdata("libdata.csv")

#May
may <- specific_month(lib,2019,"05")
numcheck(may)
may <- may[-c(43),]
numcheck(may)
find_double_entry(may)
may[89,6] <- may[97,6]
may <- may[-c(83),]
find_double_entry(may)
findmissing(may)
may <- addcolumn(may)

#April
april <- specific_month(lib,2019,"04")
numcheck(april)
april[99,18] <- 1
april <- april[-c(110),]
april[143,15] <- 0
numcheck(april)
find_double_entry(april)
april[3,19] <- april[12,19]
april <- april[-c(50,232),]
find_double_entry(april)
findmissing(april)
april <- addcolumn(april)


#March
march <- specific_month(lib,2019,"03")
march[25,11] <- 0
march[35,19] <- march[42,19]
march <- march[-c(181),]
march <- addcolumn(march)

#February
febrary <- specific_month(lib,2019,"02")
febrary <- febrary[-c(4,93,97),]
febrary[104,18] <- 13
febrary[166,13] <- 0
febrary[68,6] <- febrary[76,6]
febrary <- febrary[-c(52,134),]
febrary <- addcolumn(febrary)

#January
january <- specific_month(lib,2019,"01")
see <- january[,c(4,5,6,19)]
january[111,19] <- see[129,4]
january <- january[-c(117),]
january <- addcolumn(january)

exportxlsx(january,febrary,march,april,may)

