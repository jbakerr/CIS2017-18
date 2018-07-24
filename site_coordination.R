macdatawd <- "/Volumes/GoogleDrive/Team Drives/Data/CISDM Files/"
windowsdatawd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}

site_coordination <-readWorksheetFromFile('site_coordination.xlsx', sheet=1, header = T, startRow = 1)

#site_coordination <- data.frame(apply(site_coordination, 2, function(x) gsub("^$|^ $", NA, x)))
#site_coordination  <- site_coordination[,colSums(is.na(site_coordination))<nrow(site_coordination)]

site_coordination <- site_coordination[!is.na(site_coordination$School), ]

#colnames(site_coordination)[15:18] <- c("students_served", "parents_served", "other_served", "volunteers")


mac_save_wd <- "/Volumes/GoogleDrive/Team Drives/Data/Generated Files/"
windows_save_wd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(mac_save_wd)){
  setwd(file.path(mac_save_wd))
} else { 
  if(file.exists(windows_save_wd)){
    setwd(file.path(windows_save_wd))
  }
}

write.csv(site_coordination, "site_coordination.csv")

