grades <- readWorksheetFromFile('Student_Metric_Import_Template_Final copy 2.xlsx', sheet=7, header = T, startRow = 1)
shepard_attendance <- readWorksheetFromFile('Student_Metric_Import_Template_Final copy 2.xlsx', sheet=1, header = T, startRow = 1)

grades[grades$Core.Course == "Eng/Lang Arts/Reading/Writing", 1] <- "ELA"
grades[grades$Core.Course == "Math 1", 1] <- "Math"

quarter <- c("Q1", "Q2", "Q3", "Q4")
# grades <- spread(grades, Grading.period,  Numeric.Grade)
# progress <- spread(progress[, ! colnames(progress) %in% c("Metric", "Period")], quartersubject, Value)



colnames(grades)[1] <- "Metric"
colnames(grades)[3] <- "Q3"

grades[4:12] <- NULL

shepard_attendance$Q3 <- 0
shepard_attendance$Q3 <- (1 - (shepard_attendance$X..of.unexcused / shepard_attendance$X..of.days.in.report.period))*100
shepard_attendance$Metric <- "Attendance Rate"
shepard_attendance[2:12] <- NULL

progress <- merge(progress, grades, by = c("Student.ID", "Metric"), all = T)






progress$Q3.y[ is.na(progress$Q3.y) ] <- progress$Q3.x[ is.na(progress$Q3.y) ]

colnames(progress)[22] <- "Q3"

progress$Q3.x <- NULL
# 
progress <- merge(progress, shepard_attendance, by = c("Student.ID", "Metric"), all = T)
# 
# 
progress$Q3.y[ is.na(progress$Q3.y) ] <- progress$Q3.x[ is.na(progress$Q3.y) ]

colnames(progress)[22] <- "Q3"

progress$Q3.x <- NULL
