> tripple <- read.csv('triple.csv', header = F, colClasses = "character")
tripple <- read.csv('triple.csv', header = F, colClasses = "character")
> tripple.expanded <- tripple[rep(row.names(tripple), 3), 1:13]
> 
  > View(tripple.expanded)
> write.xlsx(tripple.expanded, 'tripple.xlsx')
> tripple.expanded <- tripple[rep(row.names(tripple), 1), 1:13]
> 
  
  
  
  library(XLConnect)

setwd("~/Code/CIS-2016-17")


progressing <- "Progressing"
declined <- "Declined"

suspension <-  readWorksheetFromFile('Eno_data.xlsx', sheet = 1, header = T )

grades <-  readWorksheetFromFile('Eno_data.xlsx', sheet = 3, header = T )

grades$Date[grades$Grading.period == 1] <- as.Date("10/27/2017", "%m/%d/%Y")
grades$Date[grades$Grading.period == 2] <- as.Date("01/16/2018", "%m/%d/%Y")


writeWorksheetToFile("Eno_data_CL.xlsx", grades, sheet = "Grades")

table(grades$Student.ID)
