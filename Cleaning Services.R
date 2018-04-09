macdatawd <- "/Volumes/GoogleDrive/My Drive/Data Files"
windowsdatawd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}


# This is the name I have been using for the detailed service dataset, however we may want to start using a different naming convention
# This is the "Tier II and III Support Detail" report. 
# **** IMPORTANT ***** delete the last row that sums all hours before saving the excel file. That row will cause problems
#data<-('services.csv', skip = 4, header = T)

data <-readWorksheetFromFile('services.xlsx', sheet=1, header = T, startRow = 2)
data <- data.frame(apply(data, 2, function(x) gsub("^$|^ $", NA, x)))

data  <- data[,colSums(is.na(data))<nrow(data)]

#data<-readWorksheetFromFile('services.xlsx', sheet=1, header = T, startRow = 10)
colnames(data)[1:2] <- c("Home.School","Student.ID")
data <- data[!is.na(data$Student.ID), ]# get rid of accidental blank rows
#data <- data[-nrow(data),] #removes summation row


#removing Nina's test data
#data <- subset(data, !data$Entered.By == "nina")
drops <- c("Provider.Type.2","ProviderUserType3", "Donation.Value", "Total.Value.of.Time")
data <- data[, ! (names(data) %in% drops)]


d <- data %>% group_by(Home.School, Entry.Date, Support.Date, Provider.Type.1, Activity, Student.Support.Category, 
                       Hours, Tier) %>% summarize(groupsize = n())
#d$groupsize[(!is.na(d$Individual.or.Group)) & d$Individual.or.Group == "Individual"] <- 1

data <- merge(data, d, by = c("Home.School", "Entry.Date", "Support.Date", "Student.Support.Category",
                               "Hours", "Tier", "Activity"))

data_test <- data
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

data$Hours <- as.numeric.factor(data$Hours)
# data$groupsize <- as.numeric.factor(data$groupsize)


data$hoursspent <- data$Hours/data$groupsize





#Saving Data

write.csv(data, "Services.csv")

