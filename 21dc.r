dc_attendace <- data

schools <- c("EK Powe Elementary School", "Merrick-Moore", "Eno Valley Elementary")

dc_attendace <- dc_attendace[,c(1,3, 7, 8, 9, 11)]
dc_attendace <- subset(dc_attendace, Activity == "21st Century Afterschool")
dc_attendace$Support.Date <- as.Date(dc_attendace$Support.Date)
dc_attendace$Support.Date <- as.factor(dc_attendace$Support.Date)
dc_attendace$Support.Date <- factor(dc_attendace$Support.Date)
dc_attendace$Home.School <- factor(dc_attendace$Home.School)


dc_attendace$Attend <- "Yes"
dc_attendace$Activity <- NULL

caselist_short <- subset(caselist, School %in% schools)
caselist_short <- caselist_short[,c(1:3,5)]
caselist_short$School <- factor(caselist_short$School)
colnames(caselist_short)[1] <- "Home.School"

caselist_short <- ddply(caselist_short,"Home.School",transform,ID=seq(from=1,by=1,length.out=length(Home.School)))



# test <- merge(caselist_short, dc_attendace, by = c("Student.ID", "Student"), all.y = TRUE)
sub <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), colnames(dc_attendace))

for (i in levels(dc_attendace$Support.Date)){
  date_attend <- dc_attendace[ dc_attendace$Support.Date == i, ]
  student <- caselist_short[!(caselist_short$Student.ID %in% date_attend$Student.ID),]
  row <- data.frame(Home.School = student$Home.School, Support.Date = i, Student.ID = student$Student.ID, Student = student$Student, Grade.Level = student$Grade.Level, Attend = "No", id= student$ID)
    
  sub <- rbind(sub, row)  
}


write.csv(sub, "21dc.csv")


