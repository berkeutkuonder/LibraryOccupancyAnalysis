importdata <- function(name) {
  library(readr)
  setwd("C:/Users/ABRA/Desktop/Junior 2019-2020/Student Associates/Library/Data Analysis")
  filename <- paste("C:/Users/ABRA/Desktop/Junior 2019-2020/Student Associates/Library/Data Analysis/",name,sep = "")
  lib <- read_csv(filename)
  lib1 <- lib[-c(2),c(5:8,19:33)]
  colnames(lib1) <- c(1:19)
  lib2 <- lib1[lib1$`3`!= "False",]
  libhead <- c("Weekday or weekend?","Weekend time","Level1","Stacks","GSR1","GSR2","GSR3","GSR4","GSR5","GSR6","GSR7","ComputerLab","Level2","24H Space","Weekday time")
  lib2[1,c(5:19)] <- libhead
  colnames(lib2) <- c(1:19)
  return(lib2)
}

specific_month <- function(lib,year,month) {
  date <- paste(year,"-",month, sep = "")
  libhead <- lib[1,]
  lib2 <- lib[substr(lib$`4`,1,7)== date,]
  lib3 <- rbind(libhead,lib2)
  row.names(lib3) <- NULL
  return(lib3)
}

numcheck <- function(d1) {
  capacity <- function(place) {
    if (place == 7) {
      return(38)
    }
    else if (place == 8) {
      return(16)
    }
    else if (place == 9) {
      return(4)
    }
    else if (place == 10) {
      return(4)
    }
    else if (place == 11) {
      return(4)
    }
    else if (place == 12) {
      return(5)
    }
    else if (place == 13) {
      return(5)
    }
    else if (place == 14) {
      return(7)
    }
    else if (place == 15) {
      return(7)
    }
    else if (place == 16) {
      return(26)
    }
    else if (place == 17) {
      return(28)
    }
    else if (place == 18) {
      return(46)
    }
    else {
      message("Inputs are wrong!")
    }
  }
  select <- function(e1,e2) {
    if (is.na(e1)) {
      return(e2)
    }
    else {
      return(e1)
    }
  }
  data <- d1[-c(1),]
  for (j in 1:nrow(data)) { 
    for (i in 7:18){
      if (is.na(as.numeric(data[j,i]))){
        message(sprintf("Not numeric!\nRow number:%d Column Number:%d (%s)",(j+1),i,d1[1,i]))
        message(sprintf("Date: %s Shift: %s",data[j,4],select(data[j,6],data[j,19])))
        message(sprintf("Entered: %s\n-",data[j,i]))
      }
      else {
        amount <- as.numeric(data[j,i])
        cap <- capacity(i)
        if(amount > cap) {
          message(sprintf("Over Capacity!\nRow number:%d Column Number:%d (%s)",(j+1),i,d1[1,i]))
          message(sprintf("Date: %s Shift: %s",data[j,4],select(data[j,6],data[j,19])))
          message(sprintf("Capacity: %d Entered: %d \n-",cap,amount))
        }
      }
    }
  }
} 

find_double_entry <- function(lib) {
  Found <- FALSE
  a <- lib$`6` #weekend check
  prev <- ""
  for (i in 1:length(a)) { 
    if (!is.na(a[i]) & !is.na(prev) & a[i]==prev) {
      print("Duplicate found!")
      print(paste("Row number:", i))
      print(substr(lib[i,4],1,20))
      print(substr(lib[i,6],1,20))
      print("")
      Found <- TRUE
    }
    prev <- a[i]
  }
  b <- lib$`19` #weekday check
  prev <- ""
  for (i in 1:length(b)) { 
    if (!is.na(b[i]) & !is.na(prev) & b[i]==prev) {
      print("Duplicate found!")
      print(paste("Row number:", i))
      print(substr(lib[i,4],1,20))
      print(substr(lib[i,19],1,20))
      print("")
      Found <- TRUE
    }
    prev <- b[i]
  }
  if (!Found) {
    print("No duplicates found!")
  }
}

findmissing <- function(input) {
  weekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
               "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  weekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
               "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  dataset <- input[-c(1),]
  days <- c()
  a <- 1
  expected <- 0
  missing <- 0
  for (item in dataset$`4`) {
    day <- substr(item,9,10)
    if (day %in% days ) {
      next
    }
    else {
      days[a] <- day
      a <- a+1
      sublib <- dataset[substr(dataset$`4`,9,10)==day,]
      type <- toString(sublib[1,5])
      if (type == "Weekday") {
        expected <- expected + 9
        if (dim(sublib)[1] == 9){
        }
        else {
          subday <- c()
          b <- 1
          for (shift in sublib$`19`){
            subday[b] <- shift
            b <- b+1
          }
          print(day)
          diff <- setdiff(weekday, subday)
          print(diff)
          missing <- missing + length(diff)
          
        }
      }
      else { #if weekend
        expected <- expected + 8
        if (dim(sublib)[1] == 8){
        }
        else {
          subday <- c()
          b <- 1
          for (shift in sublib$`6`){
            subday[b] <- shift
            b <- b+1
          }
          print(day)
          diff <- setdiff(weekend, subday)
          print(diff)
          missing <- missing + length(diff)
        }
      }
    }
  }
  message(sprintf("The expected number of entry is: %i", expected))
  message(sprintf("The actual number of entry is: %i", (expected-missing)))
  message(sprintf("The number of missing entry is: %i", missing))
  perc <- 100*(missing/expected)
  message(sprintf("The percentage of missing entry is: %f",perc))
}

addcolumn <- function(lib) { #EKLENECEK V2'YE
  lib2 <- lib
  for (i in 1:nrow(lib2)) {
    item <- c()
    a <- 1
    for (j in 7:18) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,20] <- NA
    }
    else {
      lib2[i,20] <- sum(item)
    }
    item <- c()
    a <- 1
    for (j in 9:11) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,21] <- NA
    }
    else {
      lib2[i,21] <- sum(item)
    }
    item <- c()
    a <- 1
    for (j in 12:13) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,22] <- NA
    }
    else {
      lib2[i,22] <- sum(item)
    }
    item <- c()
    a <- 1
    for (j in 14:15) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,23] <- NA
    }
    else {
      lib2[i,23] <- sum(item)
    }
  }
  libhead <- c("Weekday or weekend?","Weekend time","Level1","Stacks","GSR1","GSR2","GSR3","GSR4","GSR5","GSR6","GSR7","ComputerLab","Level2","24H Space","Weekday time","Total","GSR1-3","GSR4-5","GSR6-7")
  lib2[1,c(5:23)] <- libhead
  colnames(lib2) <- c(1:23)
  return(lib2)
}

weekend_data <- function(lib) {
  libhead <- lib[1,]
  lib2 <- lib[lib$`5`== "Weekend",]
  lib3 <- rbind(libhead,lib2)
  return(lib3)
}

weekday_data <- function(lib) {
  libhead <- lib[1,]
  lib2 <- lib[lib$`5`== "Weekday",]
  lib3 <- rbind(libhead,lib2)
  return(lib3)
}

Graph1 <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`20`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwday(d1)
  m2 <- datafinderTIMEwday(d2)
  m3 <- datafinderTIMEwday(d3)
  m4 <- datafinderTIMEwday(d4)
  m5 <- datafinderTIMEwday(d5)
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/190)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments
Graph2 <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`20`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwend(d1)
  m2 <- datafinderTIMEwend(d2)
  m3 <- datafinderTIMEwend(d3)
  m4 <- datafinderTIMEwend(d4)
  m5 <- datafinderTIMEwend(d5)
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/190)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments

Graph3 <- function(d1) {
  datafinderLEVEL1day <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`7`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderSTACKSday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`8`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderGSR13day <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`21`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderGSR45day <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`22`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderGSR67day <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`23`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderLEVEL2day <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`17`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderCLABday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`16`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinder24Hday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`18`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  c1 <- datafinderLEVEL1day(d1) 
  c2 <- datafinderSTACKSday(d1)  
  c3 <- datafinderGSR13day(d1)  
  c4 <- datafinderGSR45day(d1)  
  c5 <- datafinderGSR67day(d1)  
  c6 <- datafinderLEVEL2day(d1)  
  c7 <- datafinderCLABday(d1)  
  c8 <- datafinder24Hday(d1)  
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,c1,c2,c3,c4,c5,c6,c7,c8)
  colnames(b) <- c("Level1","Stacks","GSR1-3","GSR4-5",
                   "GSR6-7","Level2","Comp Lab","24H")
  return(b)
} 
Graph4 <- function(d1) {
  datafinderLEVEL1end <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`7`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderSTACKSend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`8`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderGSR13end <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`21`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderGSR45end <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`22`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderGSR67end <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm") 
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`23`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderLEVEL2end <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`17`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinderCLABend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`16`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  datafinder24Hend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`18`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  c1 <- datafinderLEVEL1end(d1) 
  c2 <- datafinderSTACKSend(d1) 
  c3 <- datafinderGSR13end(d1) 
  c4 <- datafinderGSR45end(d1) 
  c5 <- datafinderGSR67end(d1)
  c6 <- datafinderLEVEL2end(d1) 
  c7 <- datafinderCLABend(d1) 
  c8 <- datafinder24Hend(d1) 
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,c1,c2,c3,c4,c5,c6,c7,c8)
  colnames(b) <- c("Level1","Stacks","GSR1-3","GSR4-5",
                   "GSR6-7","Level2","Comp Lab","24H")
  return(b)
} 

GraphLevel1Day <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`7`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwday(d1)
  m2 <- datafinderTIMEwday(d2)
  m3 <- datafinderTIMEwday(d3)
  m4 <- datafinderTIMEwday(d4)
  m5 <- datafinderTIMEwday(d5)
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/38)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments
GraphLevel1End <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`7`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwend(d1)
  m2 <- datafinderTIMEwend(d2)
  m3 <- datafinderTIMEwend(d3)
  m4 <- datafinderTIMEwend(d4)
  m5 <- datafinderTIMEwend(d5)
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/38)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments

GraphStacksDay <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`8`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwday(d1)
  m2 <- datafinderTIMEwday(d2)
  m3 <- datafinderTIMEwday(d3)
  m4 <- datafinderTIMEwday(d4)
  m5 <- datafinderTIMEwday(d5)
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/16)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments
GraphStacksEnd <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`8`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwend(d1)
  m2 <- datafinderTIMEwend(d2)
  m3 <- datafinderTIMEwend(d3)
  m4 <- datafinderTIMEwend(d4)
  m5 <- datafinderTIMEwend(d5)
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/16)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments

GraphGSR13Day <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`21`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwday(d1)
  m2 <- datafinderTIMEwday(d2)
  m3 <- datafinderTIMEwday(d3)
  m4 <- datafinderTIMEwday(d4)
  m5 <- datafinderTIMEwday(d5)
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/12)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments
GraphGSR13End <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`21`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwend(d1)
  m2 <- datafinderTIMEwend(d2)
  m3 <- datafinderTIMEwend(d3)
  m4 <- datafinderTIMEwend(d4)
  m5 <- datafinderTIMEwend(d5)
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/12)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments

GraphGSR45Day <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`22`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwday(d1)
  m2 <- datafinderTIMEwday(d2)
  m3 <- datafinderTIMEwday(d3)
  m4 <- datafinderTIMEwday(d4)
  m5 <- datafinderTIMEwday(d5)
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/10)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments
GraphGSR45End <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`22`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwend(d1)
  m2 <- datafinderTIMEwend(d2)
  m3 <- datafinderTIMEwend(d3)
  m4 <- datafinderTIMEwend(d4)
  m5 <- datafinderTIMEwend(d5)
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/10)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments

GraphGSR67Day <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`23`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwday(d1)
  m2 <- datafinderTIMEwday(d2)
  m3 <- datafinderTIMEwday(d3)
  m4 <- datafinderTIMEwday(d4)
  m5 <- datafinderTIMEwday(d5)
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/14)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments
GraphGSR67End <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`23`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwend(d1)
  m2 <- datafinderTIMEwend(d2)
  m3 <- datafinderTIMEwend(d3)
  m4 <- datafinderTIMEwend(d4)
  m5 <- datafinderTIMEwend(d5)
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/14)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments

GraphLevel2Day <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`17`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwday(d1)
  m2 <- datafinderTIMEwday(d2)
  m3 <- datafinderTIMEwday(d3)
  m4 <- datafinderTIMEwday(d4)
  m5 <- datafinderTIMEwday(d5)
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/28)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
}  #needs adjustments
GraphLevel2End <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`17`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwend(d1)
  m2 <- datafinderTIMEwend(d2)
  m3 <- datafinderTIMEwend(d3)
  m4 <- datafinderTIMEwend(d4)
  m5 <- datafinderTIMEwend(d5)
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/28)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments

GraphCompDay <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`16`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwday(d1)
  m2 <- datafinderTIMEwday(d2)
  m3 <- datafinderTIMEwday(d3)
  m4 <- datafinderTIMEwday(d4)
  m5 <- datafinderTIMEwday(d5)
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/26)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments
GraphCompEnd <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`16`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwend(d1)
  m2 <- datafinderTIMEwend(d2)
  m3 <- datafinderTIMEwend(d3)
  m4 <- datafinderTIMEwend(d4)
  m5 <- datafinderTIMEwend(d5)
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/26)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments

Graph24HDay <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwday <- function(input) {
    dataset <- input[-1,]
    weekday <- dataset[dataset$`5`== "Weekday",]
    timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                     "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
    result <- c()
    for(i in 1:length(timeweekday)){
      shift <- timeweekday[i]
      subdata <- weekday[weekday$`19` == shift,]
      a <- as.numeric(subdata$`18`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwday(d1)
  m2 <- datafinderTIMEwday(d2)
  m3 <- datafinderTIMEwday(d3)
  m4 <- datafinderTIMEwday(d4)
  m5 <- datafinderTIMEwday(d5)
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/46)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments
Graph24HEnd <- function(d1,d2,d3,d4,d5) {
  datafinderTIMEwend <- function(input) {
    dataset <- input[-1,]
    weekend <- dataset[dataset$`5`== "Weekend",]
    timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                     "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
    result <- c()
    for(i in 1:length(timeweekend)){
      shift <- timeweekend[i]
      subdata <- weekend[weekend$`6` == shift,]
      a <- as.numeric(subdata$`18`)
      s <- sum(a, na.rm = TRUE)
      len <- length(a[!is.na(a)])
      avg <- s/len
      result[i] <- avg
    }
    return(result)
  }
  m1 <- datafinderTIMEwend(d1)
  m2 <- datafinderTIMEwend(d2)
  m3 <- datafinderTIMEwend(d3)
  m4 <- datafinderTIMEwend(d4)
  m5 <- datafinderTIMEwend(d5)
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,m1,m2,m3,m4,m5)
  colnames(b) <- c("January","February","March","April","May")
  c <- (b/46)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} #needs adjustments

exportxlsx <- function(d1,d2,d3,d4,d5) {
  convertnumeric <- function(v1) {
    result <- c()
    for (i in 1:nrow(v1[1])) {
      result[i] <- as.numeric(v1[i,1])
    }
    return(result)
  }
  library("xlsx")
  
  data_print <- rbind(d1,d2,d3,d4,d5)
  
  data_weekday <- weekday_data(data_print)
  colnames(data_weekday) <- data_weekday[1,]
  data_weekday <- data_weekday[-c(1),-c(1,2,3,5,6)]
  data_weekday <- data_weekday[,c(1,14,2:13,15:18)]
  for (i in 3:18) {
    data_weekday[,i] <- convertnumeric(data_weekday[,i])
  }
  
  fig1 <- Graph1(d1,d2,d3,d4,d5)
  
  data_weekend <- weekend_data(data_print)
  colnames(data_weekend) <- data_weekend[1,]
  data_weekend <- data_weekend[-c(1),-c(1,2,3,5,19)]
  for (i in 3:18) {
    data_weekend[,i] <- convertnumeric(data_weekend[,i])
  }
  
  fig2 <- Graph2(d1,d2,d3,d4,d5)
  
  month_weekday <- weekday_data(d5)
  colnames(month_weekday) <- month_weekday[1,]
  month_weekday <- month_weekday[-c(1),-c(1,2,3,5,6)]
  month_weekday <- month_weekday[,c(1,14,2:13,15:18)]
  for (i in 3:18) {
    month_weekday[,i] <- convertnumeric(month_weekday[,i])
  }
  
  fig3 <- Graph3(d5)
  
  month_weekend <- weekend_data(d5)
  colnames(month_weekend) <- month_weekend[1,]
  month_weekend <- month_weekend[-c(1),-c(1,2,3,5,19)]
  for (i in 3:18) {
    month_weekend[,i] <- convertnumeric(month_weekend[,i])
  }
  
  fig4 <- Graph4(d5)
  
  write.xlsx(as.data.frame(data_weekday), file = "data.xlsx",sheetName = "Data_Figure1", append = FALSE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig1, file = "data.xlsx",sheetName="Graph_Figure1", append=TRUE)
  write.xlsx(as.data.frame(data_weekend), file = "data.xlsx",sheetName="Data_Figure2", append=TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig2, file = "data.xlsx",sheetName="Graph_Figure2", append=TRUE)
  write.xlsx(as.data.frame(month_weekday), file = "data.xlsx",sheetName="Data_Figure3", append=TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig3, file = "data.xlsx",sheetName="Graph_Figure3", append=TRUE)
  write.xlsx(as.data.frame(month_weekend), file = "data.xlsx",sheetName="Data_Figure4", append=TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig4, file = "data.xlsx",sheetName="Graph_Figure4", append=TRUE)
  
  fig5 <- GraphLevel1Day(d1,d2,d3,d4,d5)
  fig6 <- GraphLevel1End(d1,d2,d3,d4,d5)
  
  write.xlsx(fig5, file = "data.xlsx",sheetName="Level1Day", append=TRUE)
  write.xlsx(fig6, file = "data.xlsx",sheetName="Level1End", append=TRUE)
  
  fig7 <- GraphStacksDay(d1,d2,d3,d4,d5) 
  fig8 <- GraphStacksEnd(d1,d2,d3,d4,d5)
  
  write.xlsx(fig7, file = "data.xlsx",sheetName="StacksDay", append=TRUE)
  write.xlsx(fig8, file = "data.xlsx",sheetName="StacksEnd", append=TRUE)
  
  fig9 <- GraphGSR13Day(d1,d2,d3,d4,d5)
  fig10 <- GraphGSR13End(d1,d2,d3,d4,d5) 
  
  write.xlsx(fig9, file = "data.xlsx",sheetName="GSR13Day", append=TRUE)
  write.xlsx(fig10, file = "data.xlsx",sheetName="GSR13End", append=TRUE)
  
  fig11 <- GraphGSR45Day(d1,d2,d3,d4,d5) 
  fig12 <- GraphGSR45End(d1,d2,d3,d4,d5)
  
  write.xlsx(fig11, file = "data.xlsx",sheetName="GSR45Day", append=TRUE)
  write.xlsx(fig12, file = "data.xlsx",sheetName="GSR45End", append=TRUE)
  
  fig13 <- GraphGSR67Day(d1,d2,d3,d4,d5) 
  fig14 <- GraphGSR67End(d1,d2,d3,d4,d5)
  
  write.xlsx(fig13, file = "data.xlsx",sheetName="GSR67Day", append=TRUE)
  write.xlsx(fig14, file = "data.xlsx",sheetName="GSR67End", append=TRUE)
  
  fig15 <- GraphLevel2Day(d1,d2,d3,d4,d5)
  fig16 <- GraphLevel2End(d1,d2,d3,d4,d5)
  
  write.xlsx(fig15, file = "data.xlsx",sheetName="Level2Day", append=TRUE)
  write.xlsx(fig16, file = "data.xlsx",sheetName="Level2End", append=TRUE)
  
  fig17 <- GraphCompDay(d1,d2,d3,d4,d5) 
  fig18 <- GraphCompEnd(d1,d2,d3,d4,d5)
  
  write.xlsx(fig17, file = "data.xlsx",sheetName="CompDay", append=TRUE)
  write.xlsx(fig18, file = "data.xlsx",sheetName="CompEnd", append=TRUE)
  
  fig19 <- Graph24HDay(d1,d2,d3,d4,d5)
  fig20 <- Graph24HEnd(d1,d2,d3,d4,d5)
  
  write.xlsx(fig19, file = "data.xlsx",sheetName="24HDay", append=TRUE)
  write.xlsx(fig20, file = "data.xlsx",sheetName="24HEnd", append=TRUE)
  
  
} #needs adjustments


