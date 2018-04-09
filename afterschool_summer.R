#install.packages("dplyr") # These packages need to be installed once, after that they can be called up using library. This package is for restructuring data
#install.packages("XLConnect") # This package is for loading / writing excel spreadsheets
#install.packages("tidyr")
#install.packages("plyr")
library(plyr) # ****IMPORTANT**** Load plyr before dplyr- they have some of the same named functions, and if you load in a different order it will cause problems
library(dplyr)
library(tidyr)
library(XLConnect)
###################################      SERVICES DATA CHECK      ##########################################
# Set the working directory to the local folder containing dataset. Can be done manually
# by going to Session -> Set Working Directory -> Choose Directory 
macdatawd <- "~/Google Drive/Data Files/2015-16"
windowsdatawd <- "C:/Users/USER/Google Drive/Data Files/2015-16"
if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}


eno_summer <- read.csv('eno_summer_2016.csv', header = T)
eno_summer[,10] <- NULL
eno_summer <- eno_summer[-c(1:3),]


ek_summer <- read.csv('ek_summer_2016.csv', header = T)
ek_summer[,(7:10)] <- list(NULL)
ek_summer <- ek_summer[complete.cases(ek_summer),]

ek_subset <- ek_summer[,c(2,4,5)]
colnames(ek_subset) <- c("Student", "Date", "Case_Manger")
eno_subset <- eno_summer[,c(2:4)]
colnames(eno_subset) <- c("Student", "Date", "Case_Manger")



full_subset <- rbind(eno_subset, ek_subset)

full_subset <- full_subset %>% 
  mutate(Student = strsplit(as.character(Student), ",")) %>% 
  unnest(Student)



aggregate(Date ~ Student, data = full_subset, FUN = length)

eno_test <- full_subset[full_subset$Case_Manger == "Crystal Avent",]

eno_test_1<- table(eno_test$Student,eno_test$Date)

write.csv(eno_test_1, "eno_test.csv")


ek_test <- full_subset[full_subset$Case_Manger == "Zenovia Hogue",]

ek_test_1<- table(ek_test$Student,ek_test$Date)

write.csv(ek_test_1, "ek_test.csv")



count(full_subset$Student, 'full_subset$Case_Manger')

full_subset %>% group_by(full_subset$Case_Manger,full_subset$Student) %>% tally()

test <- data.frame ( table (full_subset$Student, full_subset$Case_Manger ) )

write.csv(test, "summer.csv")
