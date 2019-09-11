importdata <- function(name) { #Imports raw data, deletes unused columns, proper column names are on first row
  library(readr)
  setwd("C:/Users/ABRA/Desktop/Junior 2019-2020/Student Associates/Library/Data Analysis")
  filename <- paste("C:/Users/ABRA/Desktop/Junior 2019-2020/Student Associates/Library/Data Analysis/",name,sep = "")
  lib <- read_csv(filename)
  lib1 <- lib[-c(2),c(5:8,19:157)]
  lib2 <- lib1[lib1$`Finished`!= "False",]
  lib2[1,5] <- "Weekday or weekend?"; lib2[1,6] <- "WeekendTime"
  for (i in 7:60) {
    a <- as.character(lib2[1,i])
    a <- substr(a,nchar(a)-10,nchar(a))
    lib2[1,i] <- a
  }
  lib2[1,61] <- "GSR1"; lib2[1,62] <- "GSR2"; lib2[1,63] <- "GSR3"
  for (i in 64:137) {
    a <- as.character(lib2[1,i])
    a <- substr(a,nchar(a)-10,nchar(a))
    lib2[1,i] <- a
  }
  lib2[1,138] <- "GSR4"; lib2[1,139] <- "GSR5"; lib2[1,140] <- "GSR6";
  lib2[1,141] <- "GSR7"; lib2[1,142] <- "CompLab"; lib2[1,143] <- "WeekdayTime"
  colnames(lib2) <- c(1:143)
  return(lib2)
}

specific_month <- function(lib,year,month) { #Creates a data frame for a specific month(string number)
  date <- paste(year,"-",month, sep = "")
  libhead <- lib[1,]
  lib2 <- lib[substr(lib$`4`,1,7)== date,]
  lib3 <- rbind(libhead,lib2)
  row.names(lib3) <- NULL
  return(lib3)
}

find_double_entry <- function(lib) {#Finds double entries and prints its row and col number
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
  b <- lib$`143` #weekday check
  prev <- ""
  for (i in 1:length(b)) { 
    if (!is.na(b[i]) & !is.na(prev) & b[i]==prev) {
      print("Duplicate found!")
      print(paste("Row number:", i))
      print(substr(lib[i,4],1,20))
      print(substr(lib[i,143],1,20))
      print("")
      Found <- TRUE
    }
    prev <- b[i]
  }
  if (!Found) {
    print("No duplicates found!")
  }
}

numcheck <- function(d1) {#Checks for NA or unvalid input/ Checks for capacity limits as well
  capacity <- function(place) {
    if (place == 61) {return(4)}
    else if (place == 62) {return(4)}
    else if (place == 63) {return(4)}
    else if (place == 138) {return(5)}
    else if (place == 139) {return(5)}
    else if (place == 140) {return(7)}
    else if (place == 141) {return(7)}
    else if (place == 142) {return(26)}
    else {message("Inputs are wrong!")}
  }
  select <- function(e1,e2) {
    if (is.na(e1)) {return(e2)}
    else {return(e1)}
  }
  data <- d1[-c(1),]
  for (j in 1:nrow(data)) { 
    for (i in c(c(61:63),c(138:142))){
      if (is.na(as.numeric(data[j,i]))){
        message(sprintf("Not numeric!\nRow number:%d Column Number:%d (%s)",(j+1),i,d1[1,i]))
        message(sprintf("Date: %s Shift: %s",data[j,4],select(data[j,6],data[j,143])))
        message(sprintf("Entered: %s\n-",data[j,i]))
      }
      else {
        amount <- as.numeric(data[j,i])
        cap <- capacity(i)
        if(amount > cap) {
          message(sprintf("Over Capacity!\nRow number:%d Column Number:%d (%s)",(j+1),i,d1[1,i]))
          message(sprintf("Date: %s Shift: %s",data[j,4],select(data[j,6],data[j,143])))
          message(sprintf("Capacity: %d Entered: %d \n-",cap,amount))
        }
      }
    }
  }
} 

findmissing <- function(input) {#Finds missing entries
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
          for (shift in sublib$`143`){
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

count_aggregate <- function(lib) {#For each space, counts the amount of like/dislike/neutral
  # Main Stack (MS)
  temp <- data.frame("MS_Like" = rep(0,nrow(lib)), "MS_Dislike" = rep(0,nrow(lib)), "MS_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 7:22) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Individual Seat Level 1 (IS_L1)
  temp <- data.frame("IS_L1_Like" = rep(0,nrow(lib)), "IS_L1_Dislike" = rep(0,nrow(lib)), "IS_L1_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 23:28) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Shared Seat Level 1 (SS_L1)
  temp <- data.frame("SS_L1_Like" = rep(0,nrow(lib)), "SS_L1_Dislike" = rep(0,nrow(lib)), "SS_L1_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 29:52) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Seat Left Level 1 (SoftL_L1)
  temp <- data.frame("SoftL_L1_Like" = rep(0,nrow(lib)), "SoftL_L1_Dislike" = rep(0,nrow(lib)), "SoftL_L1_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 53:56) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Seat Right Level 1 (SoftR_L1)
  temp <- data.frame("SoftR_L1_Like" = rep(0,nrow(lib)), "SoftR_L1_Dislike" = rep(0,nrow(lib)), "SoftR_L1_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 57:60) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Shared Seat Level 2 (SS_L2)
  temp <- data.frame("SS_L2_Like" = rep(0,nrow(lib)), "SS_L2_Dislike" = rep(0,nrow(lib)), "SS_L2_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 64:81) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Individual Seat Level 2 (SoftIS_L2)
  temp <- data.frame("SoftIS_L2_Like" = rep(0,nrow(lib)), "SoftIS_L2_Dislike" = rep(0,nrow(lib)), "SoftIS_L2_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 82:83) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Shared Seat Level 2 (SoftSS_L2)
  temp <- data.frame("SoftSS_L2_Like" = rep(0,nrow(lib)), "SoftSS_L2_Dislike" = rep(0,nrow(lib)), "SoftSS_L2_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 84:91) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Individual Seat 24H (IS_24H)
  temp <- data.frame("IS_24H_Like" = rep(0,nrow(lib)), "IS_24H_Dislike" = rep(0,nrow(lib)), "IS_24H_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 92:95) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Individual Seat 24H (SoftIS_24H)
  temp <- data.frame("SoftIS_24H_Like" = rep(0,nrow(lib)), "SoftIS_24H_Dislike" = rep(0,nrow(lib)), "SoftIS_24H_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 96:103) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Soft Shared Seat 24H (SoftSS_24H)
  temp <- data.frame("SoftSS_24H_Like" = rep(0,nrow(lib)), "SoftSS_24H_Dislike" = rep(0,nrow(lib)), "SoftSS_24H_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 104:111) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  # Shared Seat 24H (SS_24H)
  temp <- data.frame("SS_24H_Like" = rep(0,nrow(lib)), "SS_24H_Dislike" = rep(0,nrow(lib)), "SS_24H_Neutral" = rep(0,nrow(lib)))
  lib <- cbind(lib,temp)
  for (i in 1:nrow(lib)) {
    for (j in 112:137) {
      if (lib[i,j] == "Neutral") {
        lib[i,ncol(lib)] = as.numeric(lib[i,ncol(lib)]) + 1
      }
      else if (lib[i,j] == "Dislike") {
        lib[i,ncol(lib)-1] = as.numeric(lib[i,ncol(lib)-1]) + 1
      }
      else {
        lib[i,ncol(lib)-2] = as.numeric(lib[i,ncol(lib)-2]) + 1
      }
    }
  }
  lib <- lib[,-c(7:60,64:137)]
  names <- colnames(lib)[16:51]
  lib[1,16:51] <- names
  colnames(lib) <- c(1:51)
  return(lib)
}

convert_previous <- function(lib) { #Finds the total number of occupancy in 4 main spaces
  #Stacks Total Occupancy
  temp <- as.numeric(lib[-c(1),16]) + as.numeric(lib[-c(1),17])
  temp[length(temp)+1] <- "Stacks_Total"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #Stacks Dislike
  temp <- as.numeric(lib[-c(1),17]) 
  temp[length(temp)+1] <- "Stacks_DL"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #Level 1 Total Occupancy
  temp <- {as.numeric(lib[-c(1),19]) + as.numeric(lib[-c(1),20]) +
      as.numeric(lib[-c(1),22]) + as.numeric(lib[-c(1),23]) +
      as.numeric(lib[-c(1),25]) + as.numeric(lib[-c(1),26]) +
      as.numeric(lib[-c(1),28]) + as.numeric(lib[-c(1),29])}
  temp[length(temp)+1] <- "Level1_Total"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #Level 1 Dislike
  temp <- as.numeric(lib[-c(1),20]) + as.numeric(lib[-c(1),23]) + as.numeric(lib[-c(1),26]) + as.numeric(lib[-c(1),29])
  temp[length(temp)+1] <- "Level1_DL"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #Level 2 Total Occupancy
  temp <- {as.numeric(lib[-c(1),31]) + as.numeric(lib[-c(1),32]) +
      as.numeric(lib[-c(1),34]) + as.numeric(lib[-c(1),35]) +
      as.numeric(lib[-c(1),37]) + as.numeric(lib[-c(1),38]) }
  temp[length(temp)+1] <- "Level2_Total"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #Level 2 Dislike
  temp <- as.numeric(lib[-c(1),32]) + as.numeric(lib[-c(1),35]) + as.numeric(lib[-c(1),38])
  temp[length(temp)+1] <- "Level2_DL"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #24 H Total Occupancy
  temp <- {as.numeric(lib[-c(1),40]) + as.numeric(lib[-c(1),41]) +
      as.numeric(lib[-c(1),43]) + as.numeric(lib[-c(1),44]) +
      as.numeric(lib[-c(1),46]) + as.numeric(lib[-c(1),47]) +
      as.numeric(lib[-c(1),49]) + as.numeric(lib[-c(1),50])}
  temp[length(temp)+1] <- "24H_Total"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  #24 H Dislike
  temp <- as.numeric(lib[-c(1),41]) + as.numeric(lib[-c(1),44]) + as.numeric(lib[-c(1),47]) + as.numeric(lib[-c(1),50])
  temp[length(temp)+1] <- "24H_DL"
  temp <- temp[c(length(temp),1:length(temp)-1)]
  lib[,ncol(lib)+1] <- temp
  c <- lib[,c(1:6,15,7:14,52:59)]
  colnames(c) <- c(1:23)
  return(c)
}

addcolumn <- function(lib) { #Adds GSR13, GSR45,GSR67, Total columns
  lib2 <- lib
  for (i in 2:nrow(lib2)) {
    #Creating GSR13 column 
    item <- c() 
    a <- 1
    for (j in 8:10) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,24] <- NA
    }
    else {
      lib2[i,24] <- sum(item)
    }
    #Creating GSR45 column
    item <- c() 
    a <- 1
    for (j in 11:12) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,25] <- NA
    }
    else {
      lib2[i,25] <- sum(item)
    }
    #Creating GSR67 column
    item <- c() 
    a <- 1
    for (j in 13:14) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,26] <- NA
    }
    else {
      lib2[i,26] <- sum(item)
    }
    #Creating Total column
    item <- c() 
    a <- 1
    for (j in c(8:16,18,20,22)) {
      item[a] <- as.numeric(lib2[i,j])
      a <- a + 1
    }
    if (NA %in% item) {
      lib2[i,27] <- NA
    }
    else {
      lib2[i,27] <- sum(item)
    }
  }
  libhead <- c("GSR1-3","GSR4-5","GSR6-7","Total")
  lib2[1,c(24:27)] <- libhead
  colnames(lib2) <- c(1:27)
  return(lib2)
}

weekend_data <- function(lib) {#Filters for weekend data
  libhead <- lib[1,]
  lib2 <- lib[lib$`5`== "Weekend",]
  lib3 <- rbind(libhead,lib2)
  return(lib3)
}
weekday_data <- function(lib) {#Filters for weekday data
  libhead <- lib[1,]
  lib2 <- lib[lib$`5`== "Weekday",]
  lib3 <- rbind(libhead,lib2)
  return(lib3)
}

datafinderTIMEwday <- function(input) {
  dataset <- input[-1,]
  weekday <- dataset[dataset$`5`== "Weekday",]
  timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  result <- c()
  for(i in 1:length(timeweekday)){
    shift <- timeweekday[i]
    subdata <- weekday[weekday$`7` == shift,]
    a <- as.numeric(subdata$`27`)
    s <- sum(a, na.rm = TRUE)
    len <- length(a[!is.na(a)])
    avg <- s/len
    result[i] <- avg
  }
  return(result)
}
datafinderLEVEL1day <- function(input) {
  dataset <- input[-1,]
  weekday <- dataset[dataset$`5`== "Weekday",]
  timeweekday <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  result <- c()
  for(i in 1:length(timeweekday)){
    shift <- timeweekday[i]
    subdata <- weekday[weekday$`7` == shift,]
    a <- as.numeric(subdata$`18`)
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
    subdata <- weekday[weekday$`7` == shift,]
    a <- as.numeric(subdata$`16`)
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
    subdata <- weekday[weekday$`7` == shift,]
    a <- as.numeric(subdata$`24`)
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
    subdata <- weekday[weekday$`7` == shift,]
    a <- as.numeric(subdata$`25`)
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
    subdata <- weekday[weekday$`7` == shift,]
    a <- as.numeric(subdata$`26`)
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
    subdata <- weekday[weekday$`7` == shift,]
    a <- as.numeric(subdata$`20`)
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
    subdata <- weekday[weekday$`7` == shift,]
    a <- as.numeric(subdata$`15`)
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
    subdata <- weekday[weekday$`7` == shift,]
    a <- as.numeric(subdata$`22`)
    s <- sum(a, na.rm = TRUE)
    len <- length(a[!is.na(a)])
    avg <- s/len
    result[i] <- avg
  }
  return(result)
}

datafinderTIMEwend <- function(input) {
  dataset <- input[-1,]
  weekend <- dataset[dataset$`5`== "Weekend",]
  timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                   "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  result <- c()
  for(i in 1:length(timeweekend)){
    shift <- timeweekend[i]
    subdata <- weekend[weekend$`6` == shift,]
    a <- as.numeric(subdata$`27`)
    s <- sum(a, na.rm = TRUE)
    len <- length(a[!is.na(a)])
    avg <- s/len
    result[i] <- avg
  }
  return(result)
}
datafinderLEVEL1end <- function(input) {
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
datafinderSTACKSend <- function(input) {
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
datafinderGSR13end <- function(input) {
  dataset <- input[-1,]
  weekend <- dataset[dataset$`5`== "Weekend",]
  timeweekend <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                   "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  result <- c()
  for(i in 1:length(timeweekend)){
    shift <- timeweekend[i]
    subdata <- weekend[weekend$`6` == shift,]
    a <- as.numeric(subdata$`24`)
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
    a <- as.numeric(subdata$`25`)
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
    a <- as.numeric(subdata$`26`)
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
    a <- as.numeric(subdata$`20`)
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
    a <- as.numeric(subdata$`15`)
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
    a <- as.numeric(subdata$`22`)
    s <- sum(a, na.rm = TRUE)
    len <- length(a[!is.na(a)])
    avg <- s/len
    result[i] <- avg
  }
  return(result)
}

monthfinder <- function(num) {
  switch (num,
          "01" = return("January"),
          "02" = return("February"),
          "03" = return("March"),
          "04" = return("April"),
          "05" = return("May"),
          "06" = return("June"),
          "07" = return("July"),
          "08" = return("August"),
          "09" = return("September"),
          "10" = return("October"),
          "11" = return("November"),
          "12" = return("December"),
          message("Something is wrong_monthfinder")
  )
}

GraphOverallDay <- function(mons) { #Takes a list of months, returns average occupancy (weekdays) per hour for each month
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderTIMEwday(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/190)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
}
GraphOverallEnd <- function(mons) { #Takes a list of months, returns average occupancy (weekends) per hour for each month
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderTIMEwend(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/190)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
}

GraphSpacesDay <- function(mon) {
  c1 <- datafinderLEVEL1day(mon) 
  c2 <- datafinderSTACKSday(mon)  
  c3 <- datafinderGSR13day(mon)  
  c4 <- datafinderGSR45day(mon)  
  c5 <- datafinderGSR67day(mon)  
  c6 <- datafinderLEVEL2day(mon)  
  c7 <- datafinderCLABday(mon)  
  c8 <- datafinder24Hday(mon)  
  row <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
           "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  b <- data.frame(row.names = row,c1,c2,c3,c4,c5,c6,c7,c8)
  colnames(b) <- c("Level1","Stacks","GSR1-3","GSR4-5",
                   "GSR6-7","Level2","Comp Lab","24H")
  c1 <- (c1/38)*100
  c2 <- (c2/16)*100
  c3 <- (c3/12)*100
  c4 <- (c4/10)*100
  c5 <- (c5/14)*100
  c6 <- (c6/28)*100
  c7 <- (c7/26)*100
  c8 <- (c8/46)*100
  row <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  c <- data.frame(row.names = row,c1,c2,c3,c4,c5,c6,c7,c8)
  colnames(c) <- c("Level1","Stacks","GSR1-3","GSR4-5",
                   "GSR6-7","Level2","Comp Lab","24H")
  d <- rbind(b,c)
  return(d)
}
GraphSpacesEnd <- function(mon) {
  c1 <- datafinderLEVEL1end(mon) 
  c2 <- datafinderSTACKSend(mon) 
  c3 <- datafinderGSR13end(mon) 
  c4 <- datafinderGSR45end(mon) 
  c5 <- datafinderGSR67end(mon)
  c6 <- datafinderLEVEL2end(mon) 
  c7 <- datafinderCLABend(mon) 
  c8 <- datafinder24Hend(mon) 
  row <- c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
           "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  b <- data.frame(row.names = row,c1,c2,c3,c4,c5,c6,c7,c8)
  colnames(b) <- c("Level1","Stacks","GSR1-3","GSR4-5",
                   "GSR6-7","Level2","Comp Lab","24H")
  c1 <- (c1/38)*100
  c2 <- (c2/16)*100
  c3 <- (c3/12)*100
  c4 <- (c4/10)*100
  c5 <- (c5/14)*100
  c6 <- (c6/28)*100
  c7 <- (c7/26)*100
  c8 <- (c8/46)*100
  row <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  c <- data.frame(row.names = row,c1,c2,c3,c4,c5,c6,c7,c8)
  colnames(c) <- c("Level1","Stacks","GSR1-3","GSR4-5",
                   "GSR6-7","Level2","Comp Lab","24H")
  d <- rbind(b,c)
  return(d)
}

GraphLevel1Day <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderLEVEL1day(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/38)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphLevel1End <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderLEVEL1end(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/38)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

GraphStacksDay <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderSTACKSday(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/16)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphStacksEnd <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderSTACKSend(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/16)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

GraphGSR13Day <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderGSR13day(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/12)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphGSR13End <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderGSR13end(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/12)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

GraphGSR45Day <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderGSR45day(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/10)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphGSR45End <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderGSR45end(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/10)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

GraphGSR67Day <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderGSR67day(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/14)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphGSR67End <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderGSR67end(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/14)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
}

GraphLevel2Day <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderLEVEL2day(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/28)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphLevel2End <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderLEVEL2end(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/28)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

GraphCompDay <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderCLABday(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/26)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
GraphCompEnd <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinderCLABend(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/26)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

Graph24HDay <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder24Hday(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <- c("WEEKDAY - 10am","WEEKDAY - 12pm","WEEKDAY - 2pm","WEEKDAY - 4pm","WEEKDAY - 6pm",
                   "WEEKDAY - 7pm","WEEKDAY - 8pm","WEEKDAY - 9pm","WEEKDAY - 10pm")
  c <- (b/46)*100
  row.names(c) <- c("10am","12pm","2pm","4pm","6pm","7pm","8pm","9pm","10pm")
  d <- rbind(b,c)
  return(d)
} 
Graph24HEnd <- function(mons) {
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  for (i in 1:length(mons)) {
    mons[[i]] <- datafinder24Hend(mons[[i]])
  }
  b <- data.frame(mons)
  colnames(b) <- months
  rownames(b) <-  c("WEEKEND - 11am","WEEKEND - 12pm","WEEKEND - 1pm","WEEKEND - 2pm","WEEKEND - 3pm",
                    "WEEKEND - 4pm","WEEKEND - 5pm","WEEKEND - 6pm")
  c <- (b/46)*100
  row.names(c) <- c("11am","12pm","1pm","2pm","3pm","4pm","5pm","6pm")
  d <- rbind(b,c)
  return(d)
} 

findminmax <- function(lib) {
  edit <- lib[,c(7:60,64:137)]
  res <- data.frame(0,1,2,3,4, stringsAsFactors = FALSE)
  colnames(res) <- c("Seat","Like","Dislike","Neutral","Total")
  colnames(edit) <- c(1:128)
  for (i in 1:ncol(edit)) {
    like <- 0
    dislike <- 0
    neutral <- 0
    v <- as.vector(unlist(edit[-c(1),i]))
    if(i < 10) {
      res[i,1] <- substr(edit[1,i],8,11)
    }
    else if(i < 17) {
      res[i,1] <- substr(edit[1,i],7,11)
    }
    else if(i < 32) {
      res[i,1] <- substr(edit[1,i],5,11)
    }
    else if(i < 47) {
      res[i,1] <- substr(edit[1,i],4,11)
    }
    else if(i < 55) {
      res[i,1] <- edit[1,i]
    }
    else if(i < 63) {
      res[i,1] <- substr(edit[1,i],5,11)
    }
    else if(i == 63) {
      res[i,1] <- substr(edit[1,i],4,11)
    }
    else if(i == 64) {
      res[i,1] <- substr(edit[1,i],5,11)
    }
    else if(i < 73) {
      res[i,1] <- substr(edit[1,i],4,11)
    }
    else if(i < 83) {
      res[i,1] <- edit[1,i]
    }
    else if(i < 87) {
      res[i,1] <- substr(edit[1,i],4,11)
    }
    else if(i < 103) {
      res[i,1] <- paste("S",edit[1,i], sep = "")
    }
    else if(i < 112) {
      res[i,1] <- substr(edit[1,i],4,11)
    }
    else if(i < 129) {
      res[i,1] <- substr(edit[1,i],3,11)
    }
    else {
      message("Oh shit_findminmax")
      return()
    }
    for (item in v) {
      if (item == "Neutral") {
        neutral <- neutral + 1
      }
      else if (item == "Like") {
        like <- like + 1
      }
      else if (item == "Dislike") {
        dislike <- dislike + 1
      }
      else {
        message("What the f***_findminmax")
        return()
      }
    }
    res[i,2] <- like
    res[i,3] <- dislike
    res[i,4] <- neutral
    res[i,5] <- like + dislike
  }
  res <- res[order(res$Total, decreasing = TRUE),]
  return(res)
}

convertnumeric <- function(v1) {
  result <- c()
  for (i in 1:length(v1)) {
    result[i] <- as.numeric(v1[i])
  }
  return(result)
}

BarMonthsDay <- function(mons) { #All months, like dislike, bar graph, weekday
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  
  for(i in 1:length(mons)) {
    mons[[i]] <- weekday_data(mons[[i]])
    colnames(mons[[i]]) <- as.character(mons[[i]][1,])
    mons[[i]] <- mons[[i]][-c(1),-c(1,2,3,5,6)]
    for (j in 3:22) {
      mons[[i]][,j] <- convertnumeric(as.vector(unlist(mons[[i]][,j])))
    }
  }
  
  T1 <- mean(mons[[1]][,11]) + mean(mons[[1]][,13]) + mean(mons[[1]][,15]) + mean(mons[[1]][,17])
  D1 <- mean(mons[[1]][,12]) + mean(mons[[1]][,14]) + mean(mons[[1]][,16]) + mean(mons[[1]][,18])
  d <- data.frame(T1,D1)
  
  if(length(mons) > 1) {
    for(i in 2:length(mons)) {
      T1 <- mean(mons[[i]][,11]) + mean(mons[[i]][,13]) + mean(mons[[i]][,15]) + mean(mons[[i]][,17])
      D1 <- mean(mons[[i]][,12]) + mean(mons[[i]][,14]) + mean(mons[[i]][,16]) + mean(mons[[i]][,18])
      d <- rbind(d, data.frame(T1,D1))
    }
  }
  colnames(d) <- c("Total","Dislike")
  months_total <- c()
  for (i in 1:length(months)) {
    months_total[i] <- paste(months[i],"Total")
  }
  rownames(d) <- months_total
  e <- d / 128
  rownames(e) <- months
  d <- rbind(d,e)
  return(d)
}
BarMonthsEnd <- function(mons) { #All months, like dislike, bar graph, weekend
  months <- c()
  for (i in 1:length(mons)) {
    months[i] <- monthfinder(substr(mons[[i]][4,4],6,7))
  }
  
  for(i in 1:length(mons)) {
    mons[[i]] <- weekend_data(mons[[i]])
    colnames(mons[[i]]) <- as.character(mons[[i]][1,])
    mons[[i]] <- mons[[i]][-c(1),-c(1,2,3,5,6)]
    for (j in 3:22) {
      mons[[i]][,j] <- convertnumeric(as.vector(unlist(mons[[i]][,j])))
    }
  }
  
  T1 <- mean(mons[[1]][,11]) + mean(mons[[1]][,13]) + mean(mons[[1]][,15]) + mean(mons[[1]][,17])
  D1 <- mean(mons[[1]][,12]) + mean(mons[[1]][,14]) + mean(mons[[1]][,16]) + mean(mons[[1]][,18])
  d <- data.frame(T1,D1)
  
  if(length(mons) > 1) {
    for(i in 2:length(mons)) {
      T1 <- mean(mons[[i]][,11]) + mean(mons[[i]][,13]) + mean(mons[[i]][,15]) + mean(mons[[i]][,17])
      D1 <- mean(mons[[i]][,12]) + mean(mons[[i]][,14]) + mean(mons[[i]][,16]) + mean(mons[[i]][,18])
      d <- rbind(d, data.frame(T1,D1))
    }
  }
  colnames(d) <- c("Total","Dislike")
  months_total <- c()
  for (i in 1:length(months)) {
    months_total[i] <- paste(months[i],"Total")
  }
  rownames(d) <- months_total
  e <- d / 128
  rownames(e) <- months
  d <- rbind(d,e)
  
  return(d)
}

Bar4spaceDay <- function(mon) { #4 main spaces, like dislike, bar graph, weekday
  t <- c(mean(mon[,11]), mean(mon[,13]), mean(mon[,15]), mean(mon[,17]))
  d <- c(mean(mon[,12]), mean(mon[,14]), mean(mon[,16]), mean(mon[,18]))
  d <- data.frame(t,d)
  colnames(d) <- c("Total","Dislike")
  r1 <- d[1,] / 16
  r2 <- d[2,] / 38
  r3 <- d[3,] / 28
  r4 <- d[4,] / 46
  d <- rbind(d,r1,r2,r3,r4)
  rownames(d) <- c("Stacks_Total", "Level1_Total", "Level2_Total", "24H_Total","Stacks", "Level1", "Level2", "24H")
  return(d)
}
Bar4spaceEnd <- function(mon) { #4 main spaces, like dislike, bar graph, weekend
  t <- c(mean(mon[,11]), mean(mon[,13]), mean(mon[,15]), mean(mon[,17]))
  d <- c(mean(mon[,12]), mean(mon[,14]), mean(mon[,16]), mean(mon[,18]))
  d <- data.frame(t,d)
  colnames(d) <- c("Total","Dislike")
  r1 <- d[1,] / 16
  r2 <- d[2,] / 38
  r3 <- d[3,] / 28
  r4 <- d[4,] / 46
  d <- rbind(d,r1,r2,r3,r4)
  rownames(d) <- c("Stacks_Total", "Level1_Total", "Level2_Total", "24H_Total","Stacks", "Level1", "Level2", "24H")
  return(d)
}

BarMonthL1Day <- function(mon) { #Individual space analysis - Level 1 - Weekdays
  mon <- weekday_data(mon)
  IS_L1 <- c(mean(as.numeric(mon[-c(1),19])) + mean(as.numeric(mon[-c(1),20])),mean(as.numeric(mon[-c(1),20])))
  SS_L1 <- c(mean(as.numeric(mon[-c(1),22])) + mean(as.numeric(mon[-c(1),23])),mean(as.numeric(mon[-c(1),23])))
  SoftR_L1 <- c(mean(as.numeric(mon[-c(1),28])) + mean(as.numeric(mon[-c(1),29])),mean(as.numeric(mon[-c(1),29])))
  SoftL_L1 <- c(mean(as.numeric(mon[-c(1),25])) + mean(as.numeric(mon[-c(1),26])),mean(as.numeric(mon[-c(1),26])))
  d <- rbind.data.frame(IS_L1,SS_L1,SoftR_L1,SoftL_L1)
  colnames(d) <- c("Total","Dislike")
  r1 <- d[1,] / 6
  r2 <- d[2,] / 24
  r3 <- d[3,] / 4
  r4 <- d[4,] / 4
  d <- rbind(d,r1,r2,r3,r4)
  rownames(d) <- c("IS_L1_Total","SS_L1_Total","SoftR_L1_Total","SoftL_L1_Total","IS_L1","SS_L1","SoftR_L1","SoftL_L1")
  return(d)
}
BarMonthL1End <- function(mon) { #Individual space analysis - Level 1 - Weekends
  mon <- weekend_data(mon)
  IS_L1 <- c(mean(as.numeric(mon[-c(1),19])) + mean(as.numeric(mon[-c(1),20])),mean(as.numeric(mon[-c(1),20])))
  SS_L1 <- c(mean(as.numeric(mon[-c(1),22])) + mean(as.numeric(mon[-c(1),23])),mean(as.numeric(mon[-c(1),23])))
  SoftR_L1 <- c(mean(as.numeric(mon[-c(1),28])) + mean(as.numeric(mon[-c(1),29])),mean(as.numeric(mon[-c(1),29])))
  SoftL_L1 <- c(mean(as.numeric(mon[-c(1),25])) + mean(as.numeric(mon[-c(1),26])),mean(as.numeric(mon[-c(1),26])))
  d <- rbind.data.frame(IS_L1,SS_L1,SoftR_L1,SoftL_L1)
  colnames(d) <- c("Total","Dislike")
  r1 <- d[1,] / 6
  r2 <- d[2,] / 24
  r3 <- d[3,] / 4
  r4 <- d[4,] / 4
  d <- rbind(d,r1,r2,r3,r4)
  rownames(d) <- c("IS_L1_Total","SS_L1_Total","SoftR_L1_Total","SoftL_L1_Total","IS_L1","SS_L1","SoftR_L1","SoftL_L1")
  return(d)
}

BarMonth24HDay <- function(mon) { #Individual space analysis - 24H - Weekdays
  mon <- weekday_data(mon)
  IS_24h <- c(mean(as.numeric(mon[-c(1),40])) + mean(as.numeric(mon[-c(1),41])),mean(as.numeric(mon[-c(1),41])))
  SS_24h <- c(mean(as.numeric(mon[-c(1),49])) + mean(as.numeric(mon[-c(1),50])),mean(as.numeric(mon[-c(1),50])))
  SoftIS_24h <- c(mean(as.numeric(mon[-c(1),43])) + mean(as.numeric(mon[-c(1),44])),mean(as.numeric(mon[-c(1),44])))
  SoftSS_24h <- c(mean(as.numeric(mon[-c(1),46])) + mean(as.numeric(mon[-c(1),47])),mean(as.numeric(mon[-c(1),47])))
  d <- rbind.data.frame(IS_24h,SS_24h,SoftIS_24h,SoftSS_24h)
  colnames(d) <- c("Total","Dislike")
  r1 <- d[1,] / 4
  r2 <- d[2,] / 26
  r3 <- d[3,] / 8
  r4 <- d[4,] / 8
  d <- rbind(d,r1,r2,r3,r4)
  rownames(d) <- c("IS_24h_Total","SS_24h_Total","SoftIS_24h_Total","SoftSS_24h_Total","IS_24h","SS_24h","SoftIS_24h_L1","SoftSS_24h")
  return(d)
}
BarMonth24HEnd <- function(mon) { #Individual space analysis - 24H - Weekends
  mon <- weekend_data(mon)
  IS_24h <- c(mean(as.numeric(mon[-c(1),40])) + mean(as.numeric(mon[-c(1),41])),mean(as.numeric(mon[-c(1),41])))
  SS_24h <- c(mean(as.numeric(mon[-c(1),49])) + mean(as.numeric(mon[-c(1),50])),mean(as.numeric(mon[-c(1),50])))
  SoftIS_24h <- c(mean(as.numeric(mon[-c(1),43])) + mean(as.numeric(mon[-c(1),44])),mean(as.numeric(mon[-c(1),44])))
  SoftSS_24h <- c(mean(as.numeric(mon[-c(1),46])) + mean(as.numeric(mon[-c(1),47])),mean(as.numeric(mon[-c(1),47])))
  d <- rbind.data.frame(IS_24h,SS_24h,SoftIS_24h,SoftSS_24h)
  colnames(d) <- c("Total","Dislike")
  r1 <- d[1,] / 4
  r2 <- d[2,] / 26
  r3 <- d[3,] / 8
  r4 <- d[4,] / 8
  d <- rbind(d,r1,r2,r3,r4)
  rownames(d) <- c("IS_24h_Total","SS_24h_Total","SoftIS_24h_Total","SoftSS_24h_Total","IS_24h","SS_24h","SoftIS_24h_L1","SoftSS_24h")
  return(d)
}

BarMonthL2Day <- function(mon) { #Individual space analysis - Level 2 - Weekdays
  mon <- weekday_data(mon)
  SS_L2 <- c(mean(as.numeric(mon[-c(1),31])) + mean(as.numeric(mon[-c(1),32])),mean(as.numeric(mon[-c(1),32])))
  SoftIS_L2 <- c(mean(as.numeric(mon[-c(1),34])) + mean(as.numeric(mon[-c(1),35])),mean(as.numeric(mon[-c(1),35])))
  SoftSS_L2 <- c(mean(as.numeric(mon[-c(1),37])) + mean(as.numeric(mon[-c(1),38])),mean(as.numeric(mon[-c(1),38])))
  d <- rbind.data.frame(SS_L2,SoftIS_L2,SoftSS_L2)
  colnames(d) <- c("Total","Dislike")
  r1 <- d[1,] / 18
  r2 <- d[2,] / 2
  r3 <- d[3,] / 8
  d <- rbind(d,r1,r2,r3)
  rownames(d) <- c("SS_L2_Total","SoftIS_L2_Total","SoftSS_L2_Total","SS_L2","SoftIS_L2","SoftSS_L2")
  return(d)
}
BarMonthL2End <- function(mon) { #Individual space analysis - Level 2 - Weekends
  mon <- weekend_data(mon)
  SS_L2 <- c(mean(as.numeric(mon[-c(1),31])) + mean(as.numeric(mon[-c(1),32])),mean(as.numeric(mon[-c(1),32])))
  SoftIS_L2 <- c(mean(as.numeric(mon[-c(1),34])) + mean(as.numeric(mon[-c(1),35])),mean(as.numeric(mon[-c(1),35])))
  SoftSS_L2 <- c(mean(as.numeric(mon[-c(1),37])) + mean(as.numeric(mon[-c(1),38])),mean(as.numeric(mon[-c(1),38])))
  d <- rbind.data.frame(SS_L2,SoftIS_L2,SoftSS_L2)
  colnames(d) <- c("Total","Dislike")
  r1 <- d[1,] / 18
  r2 <- d[2,] / 2
  r3 <- d[3,] / 8
  d <- rbind(d,r1,r2,r3)
  rownames(d) <- c("SS_L2_Total","SoftIS_L2_Total","SoftSS_L2_Total","SS_L2","SoftIS_L2","SoftSS_L2")
  return(d)
}

exportxlsx <- function(...) {
  library("xlsx")
  a <- list(...)
  
  specific <- findminmax(a[[length(a)]])
  
  b <- count_aggregate(a[[length(a)]])
  
  fign1 <- BarMonthL1Day(b)
  fign2 <- BarMonthL1End(b)
  
  fign3 <- BarMonth24HDay(b)
  fign4 <- BarMonth24HEnd(b)
  
  fign5 <- BarMonthL2Day(b)
  fign6 <- BarMonthL2End(b)
  
  for(i in 1:length(a)) {
    a[[i]] <- count_aggregate(a[[i]])
    a[[i]] <- convert_previous(a[[i]])
    a[[i]] <- addcolumn(a[[i]])
  }
  
  data_print <- a[[1]]
  
  if(length(a) > 1) {
    for(i in 2:length(a)) {
      data_print <- rbind(data_print, a[[i]])
    }
  }
  
  data_weekday <- weekday_data(data_print)
  colnames(data_weekday) <- as.character(data_weekday[1,])
  data_weekday <- data_weekday[-c(1),-c(1,2,3,5,6)]
  for (i in 3:22) {
    data_weekday[,i] <- convertnumeric(data_weekday[,i])
  }
  
  fig1 <- GraphOverallDay(a) 
  
  data_weekend <- weekend_data(data_print)
  colnames(data_weekend) <- data_weekend[1,]
  data_weekend <- data_weekend[-c(1),-c(1,2,3,5,7)]
  for (i in 3:22) {
    data_weekend[,i] <- convertnumeric(data_weekend[,i])
  }
  
  fig2 <- GraphOverallEnd(a)
  
  fig21 <- BarMonthsDay(a)
  fig22 <- BarMonthsEnd(a)
  
  month_weekday <- weekday_data(a[[length(a)]])
  colnames(month_weekday) <- month_weekday[1,]
  month_weekday <- month_weekday[-c(1),-c(1,2,3,5,6)]
  for (i in 3:22) {
    month_weekday[,i] <- convertnumeric(month_weekday[,i])
  }
  
  fig3 <- GraphSpacesDay(a[[length(a)]])
  
  month_weekend <- weekend_data(a[[length(a)]])
  colnames(month_weekend) <- month_weekend[1,]
  month_weekend <- month_weekend[-c(1),-c(1,2,3,5,7)]
  for (i in 3:22) {
    month_weekend[,i] <- convertnumeric(month_weekend[,i])
  }
  
  fig4 <- GraphSpacesEnd(a[[length(a)]])
  
  fig41 <- Bar4spaceDay(month_weekday)
  fig42 <- Bar4spaceEnd(month_weekend)
  
  write.xlsx(as.data.frame(specific), file = "data.xlsx",sheetName = "Seats", append = FALSE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(as.data.frame(data_weekday), file = "data.xlsx",sheetName = "Data_Figure1", append = TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig1, file = "data.xlsx",sheetName="OverallDay", append=TRUE)
  write.xlsx(as.data.frame(data_weekend), file = "data.xlsx",sheetName="Data_Figure2", append=TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig2, file = "data.xlsx",sheetName="OverallEnd", append=TRUE)
  write.xlsx(fig21, file = "data.xlsx",sheetName="BarMonthsDay", append=TRUE)
  write.xlsx(fig22, file = "data.xlsx",sheetName="BarMonthsEnd", append=TRUE)
  write.xlsx(as.data.frame(month_weekday), file = "data.xlsx",sheetName="Data_Figure3", append=TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig3, file = "data.xlsx",sheetName="SpacesDay", append=TRUE)
  write.xlsx(as.data.frame(month_weekend), file = "data.xlsx",sheetName="Data_Figure4", append=TRUE,
             col.names = TRUE, row.names = FALSE)
  write.xlsx(fig4, file = "data.xlsx",sheetName="SpacesEnd", append=TRUE)
  write.xlsx(fig41, file = "data.xlsx",sheetName="Bar4spaceDay", append=TRUE)
  write.xlsx(fig42, file = "data.xlsx",sheetName="Bar4spaceEnd", append=TRUE)
  
  write.xlsx(fign1, file = "data.xlsx",sheetName="BarMonthL1Day", append=TRUE)
  write.xlsx(fign2, file = "data.xlsx",sheetName="BarMonthL1End", append=TRUE)
  write.xlsx(fign3, file = "data.xlsx",sheetName="BarMonth24HDay", append=TRUE)
  write.xlsx(fign4, file = "data.xlsx",sheetName="BarMonth24HEnd", append=TRUE)
  write.xlsx(fign5, file = "data.xlsx",sheetName="BarMonthL2Day", append=TRUE)
  write.xlsx(fign6, file = "data.xlsx",sheetName="BarMonthL2End", append=TRUE)
  
  fig5 <- GraphLevel1Day(a)
  fig6 <- GraphLevel1End(a)
  
  write.xlsx(fig5, file = "data.xlsx",sheetName="Level1Day", append=TRUE)
  write.xlsx(fig6, file = "data.xlsx",sheetName="Level1End", append=TRUE)
  
  fig7 <- GraphStacksDay(a) 
  fig8 <- GraphStacksEnd(a)
  
  write.xlsx(fig7, file = "data.xlsx",sheetName="StacksDay", append=TRUE)
  write.xlsx(fig8, file = "data.xlsx",sheetName="StacksEnd", append=TRUE)
  
  fig9 <- GraphGSR13Day(a)
  fig10 <- GraphGSR13End(a) 
  
  write.xlsx(fig9, file = "data.xlsx",sheetName="GSR13Day", append=TRUE)
  write.xlsx(fig10, file = "data.xlsx",sheetName="GSR13End", append=TRUE)
  
  fig11 <- GraphGSR45Day(a) 
  fig12 <- GraphGSR45End(a)
  
  write.xlsx(fig11, file = "data.xlsx",sheetName="GSR45Day", append=TRUE)
  write.xlsx(fig12, file = "data.xlsx",sheetName="GSR45End", append=TRUE)
  
  fig13 <- GraphGSR67Day(a) 
  fig14 <- GraphGSR67End(a)
  
  write.xlsx(fig13, file = "data.xlsx",sheetName="GSR67Day", append=TRUE)
  write.xlsx(fig14, file = "data.xlsx",sheetName="GSR67End", append=TRUE)
  
  fig15 <- GraphLevel2Day(a)
  fig16 <- GraphLevel2End(a)
  
  write.xlsx(fig15, file = "data.xlsx",sheetName="Level2Day", append=TRUE)
  write.xlsx(fig16, file = "data.xlsx",sheetName="Level2End", append=TRUE)
  
  fig17 <- GraphCompDay(a) 
  fig18 <- GraphCompEnd(a)
  
  write.xlsx(fig17, file = "data.xlsx",sheetName="CompDay", append=TRUE)
  write.xlsx(fig18, file = "data.xlsx",sheetName="CompEnd", append=TRUE)
  
  fig19 <- Graph24HDay(a)
  fig20 <- Graph24HEnd(a)
  
  write.xlsx(fig19, file = "data.xlsx",sheetName="24HDay", append=TRUE)
  write.xlsx(fig20, file = "data.xlsx",sheetName="24HEnd", append=TRUE)
  
  
}
