macdatawd <- "/Volumes/GoogleDrive/Team Drives/Data/CISDM Files/"
windowsdatawd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}

#Load Student Progress Monitoring Excel Sheet
# progress<-readWorksheetFromFile('progress.xlsx', sheet=1, header = T, startRow = 1)

nms <- names(read_excel('progress.xlsx', n_max = 0))


ct <- ifelse(grepl("Date", nms), "date", "guess")

# caselist <- read_excel(readxl_example("datasets.xlsx"), col_types = ct)

progress <- read_excel('progress.xlsx', sheet = 1,skip = 0, col_types = ct)

colnames(progress) <- make.names(colnames(progress))





#Rename Grading Quarters - make sure that the column numbers are acurate 
colnames(progress)[11:14] <- c("Q1", "Q2", "Q3", "Q4")


#Changing Metric Title - make sure that the first section in quotes is exactly as found in the excel file. 
progress$Metric[progress$Metric =='Core Course Grades: Eng/Lang Arts/Reading/Writing'] <- "ELA"
progress$Metric[progress$Metric =='Core Course Grades: Math 1'] <- "Math"
progress$Metric[progress$Metric =='Core Course Grades: Science'] <- "Science"
progress <- progress[ ! (progress$Metric =='Standardized test score: English / Language Arts'),]
progress <- progress[ ! (progress$Metric =='Standardized test score: Science'),]


progress <- progress[,c(1:8, 9:12,21,13, 14:20)]


#Changing 
colnames(progress)[4] <- c("Student")

#Dropping Unneeded Columns
progress$Latest.Progress <- NULL
progress$Target.Set.Date <- NULL
progress$Affiliate <- NULL
progress$Enrollment.End.Date <- NULL
progress$School.Year <- NULL
progress$Baseline.Date <- NULL
progress$Goal <- NULL

#removing unwanted metrics - if the column title doesn't apear in the list of metrics from the environment.r script, then they will be deleted.
progress <- subset(progress, progress$Metric %in% metrics)


#Adjusting Attendance Rate from days to percentage not needed any because of CISDM auto calculation. Leaving it in in case that changes.
# 
# elem.adjust <- progress$Q1[progress$Metric == "Attendance Rate" & !is.na(progress$Q1) & (progress$School %in% elem) & progress$Q1 < 50]
# elem.adjust <- (45-elem.adjust)/45
# elem.adjust <- elem.adjust * 100
# 
# progress$Q1[progress$Metric == "Attendance Rate" & progress$School %in% elem & progress$Q1 < 50 & !is.na(progress$Q1)] <- elem.adjust
# 
# high.adjust <- progress$Q1[progress$Metric == "Attendance Rate" & !is.na(progress$Q1) & (progress$School %in% high) & progress$Q1 < 1]
# high.adjust <- high.adjust *100
# 
# progress$Q1[progress$Metric == "Attendance Rate" & !is.na(progress$Q1) & (progress$School %in% high) & progress$Q1 < 1] <- high.adjust
# 


#Creating Names for each quarter / subject combination

progress <- gather(progress, Period, Value, Baseline:Q4, factor_key = T)
quartersubject <- paste(progress$Period, "_", progress$Metric, sep = "")
progress$quartersubject <- quartersubject


#removing duplicates
# This is a soft option that just deletes one of the duplicates arbitrarily

progress <- progress[!duplicated(progress[,c("School", "Student", "Metric", "Value", "Student.ID", "Period", "Enrollment.Status")]), ] 

#Creating a wide data frame
progress <- spread(progress[, ! colnames(progress) %in% c("Metric", "Period")], quartersubject, Value)

# Checking for incomplete progress monitoring settups. If a studuent is missing baseline or goal data they will be flagged as TRUE
progress$attend_error <- ifelse(is.na(progress$`Baseline_Attendance Rate`) & is.na(progress$`Target_Attendance Rate`), TRUE, FALSE)
progress$math_error <- ifelse(is.na(progress$Baseline_Math) & is.na(progress$Target_Math), TRUE, FALSE)
progress$science_error <- ifelse(is.na(progress$Baseline_Science) & is.na(progress$Target_Science), TRUE, FALSE)
progress$ELA_error <- ifelse(is.na(progress$Baseline_ELA) & is.na(progress$Target_ELA), TRUE, FALSE)
progress$suspension_error <- ifelse(is.na(progress$Baseline_Suspensions) & is.na(progress$Target_Suspensions), TRUE, FALSE)
progress$error <- ifelse(progress$attend_error == T | progress$math_error == T | progress$science_error == T | progress$ELA_error == T, TRUE, FALSE)

#Removing Baseline and Target data now that it isn't relevant
progress$`Baseline_Attendance Rate` <- NULL
progress$Baseline_ELA <- NULL
progress$Baseline_Math <- NULL
progress$Baseline_Science <- NULL
progress$Baseline_Suspensions <- NULL

progress$Target_ELA <- NULL
progress$`Target_Attendance Rate` <- NULL
progress$Target_Math <- NULL
progress$Target_Suspensions <- NULL 
progress$Target_Science <- NULL

#Adding Caselist 


nms <- names(read_excel('caselist.xlsx', n_max = 0))


ct <- ifelse(grepl("Date", nms), "date", "guess")

# caselist <- read_excel(readxl_example("datasets.xlsx"), col_types = ct)

caselist <- read_excel('caselist.xlsx', sheet = 1,skip = 0, col_types = ct)

colnames(caselist) <- make.names(colnames(caselist))



#caselist<-read.csv('caselist.csv', skip = 3, header = T, sep = ",")
# caselist<-readWorksheetFromFile('caselist.xlsx', sheet=1, header = T, startRow = 2)

caselist <- data.frame(apply(caselist, 2, function(x) gsub("^$|^ $", NA, x)))

caselist  <- caselist[,colSums(is.na(caselist))<nrow(caselist)]


#Changing name of columns, make sure that the column numbers are still accurate. 
colnames(caselist)[1:2] <- c("School","Student.ID")

#Changing name of the progress dataset 

caselist$Student.ID <- as.character(caselist$Student.ID)
progress$Student.ID <- as.character(progress$Student.ID)
progress$Enrollment.Status <- NULL


caselist_test <- caselist[duplicated(caselist[,c("Student.ID")]), ] # This is a soft option that just deletes one of the duplicates arbitrarily
progress_test <- progress[duplicated(progress[,c("Student.ID")]), ] # This is a soft option that just deletes one of the duplicates arbitrarily
#progress$Student <- NULL 
#caselist$Student <- NULL 

# progress$Student.ID <- NULL
# progress$Case.Manager <- NULL
progress[,c(1:2, 4:6)] <- NULL


progress <- merge(caselist, progress, by = "Student.ID", all = T)







#Adding Birth days



age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                           as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                           ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                  as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                  as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age
}


progress$Birth.Date <- as.Date(progress$Birth.Date)


progress$age <- age_years(progress$Birth.Date, Sys.Date())









