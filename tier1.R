macdatawd <- "/Volumes/GoogleDrive/Team Drives/Data/CISDM Files/"
windowsdatawd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}

tier1 <-readWorksheetFromFile('tier1.xlsx', sheet=1, header = T, startRow = 2)

tier1 <- data.frame(apply(tier1, 2, function(x) gsub("^$|^ $", NA, x)))
tier1  <- tier1[,colSums(is.na(tier1))<nrow(tier1)]

tier1 <- tier1[!is.na(tier1$School), ]

colnames(tier1)[15:18] <- c("students_served", "parents_served", "other_served", "volunteers")


mac_save_wd <- "/Volumes/GoogleDrive/Team Drives/Data/Generated Files/"
windows_save_wd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(mac_save_wd)){
  setwd(file.path(mac_save_wd))
} else { 
  if(file.exists(windows_save_wd)){
    setwd(file.path(windows_save_wd))
  }
}

write.csv(tier1, "tier1.csv")

