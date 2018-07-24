macdatawd <- "/Volumes/GoogleDrive/Team Drives/Data/CISDM Files/"
windowsdatawd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}


# Setting up 15-16 Data
stlist1516 <- read.csv('2015-16/studentlist1516.csv', header = T)

stlist1516 <- stlist1516[,c(2:6, 81, 97, 103:104,122:125)]

stlist1516$totabs <-100 -  ((stlist1516$totabs / 180) * 100)

colnames(stlist1516)[6:13] <- paste(colnames(stlist1516)[6:13], "1516", sep = "_")

colnames(stlist1516)[1] <- "Student.ID"

# Setting up 16-17 Data
stlist1617 <- read.csv('2016-17/studentlist.csv', header = T)

stlist1617 <- stlist1617[,c(2:4, 7, 10, 43, 50:51, 57,63)]

colnames(stlist1617)[6:10] <- paste(colnames(stlist1617)[6:10], "1617", sep = "_")


current_year <- stlist

current_year <- current_year[,c(1:3, 6, 9, 43, 55:56, 66, 72)]

colnames(current_year)[6:10] <- paste(colnames(current_year)[6:10], "1718", sep = "_")

# Marking students from 15-16 who continued to 16-17
stlist1516$continue <- F
stlist1516$continue <- ifelse(stlist1516$Student.ID %in% stlist1617$Student.ID, stlist1516$continue <- T, stlist1516$continue <- F)


# Marking Students who returned from 15-16
stlist1617$return_1516 <- F

stlist1617$return_1516 <- ifelse(stlist1617$Student.ID %in% stlist1516$Student.ID, stlist1617$return_1516 <- T, stlist1617$return_1516 <- F)

# Merging 15-16 into 16-17


stlist1617 <- merge(x = stlist1617, y =  stlist1516[, c(1,6:13)], by = "Student.ID", all.x = T)


# Marking students from 16-17 who continued to 17-18

stlist1617$continue <- F
stlist1617$continue <- ifelse(stlist1617$Student.ID %in% stlist$Student.ID, stlist1617$continue <- T, stlist1617$continue <- F)



current_year$return_1617 <- F
current_year$return_1617 <- ifelse(current_year$Student.ID %in% stlist1617$Student.ID, current_year$return_1617 <- T, current_year$return_1617 <- F)


# Merging 16-17 into current year

current_year <- merge(x = current_year, y =  stlist1617[, c(1,6:19)], by = "Student.ID", all.x = T)


mac_save_wd <- "/Volumes/GoogleDrive/Team Drives/Data/Generated Files/"
windows_save_wd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(mac_save_wd)){
  setwd(file.path(mac_save_wd))
} else { 
  if(file.exists(windows_save_wd)){
    setwd(file.path(windows_save_wd))
  }
}


write.csv(current_year, "returning_students.csv")





