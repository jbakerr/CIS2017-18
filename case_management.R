

case_management <- do.call("rbind", lapply(1:7, function(n) readWorksheetFromFile("case_management.xlsx", sheet = n, header = T))) 


colnames(case_management)[3] <- "Home.School"
colnames(case_management)[5] <- "Student.Support.Category"
colnames(case_management)[9] <- "Hours"
colnames(case_management)[6] <- "Student.Support.Name"
colnames(case_management)[11] <- "Provider.Name.1"
colnames(case_management)[1] <- "Support.Date"


case_management[2] <- NULL
case_management[7] <- NULL 
case_management[6] <- NULL 

#colnames(case_management) %in% colnames(data)

test <- case_management[!(case_management$Student %in% caselist$Student),]

#case_management <- case_management[-27,]

case_management <- merge(case_management, caselist[, c("Student", "Student.ID")], by="Student")
case_management[8] <- NULL
data <- rbind.fill(data, case_management)
data$Hours <- as.numeric(data$Hours)



write.csv(data, "Services.csv")
