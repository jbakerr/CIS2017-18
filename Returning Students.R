

stlist1516 <- read.csv('studentlist1516.csv', header = T)

#stlist1516 <- stlist1516[!duplicated(stlist1516[,c("Case.ID", "Name","Site")]), ] # This is a soft option that just deletes one of the duplicates arbitrarily



stlist1516$continue <- F
stlist1516$continue <- ifelse(stlist1516$Case.ID %in% stlist$Student.ID, stlist1516$continue <- T, stlist1516$continue <- F)


stlist$return <- F

stlist$return <- ifelse(stlist$Student.ID %in% stlist1516$Case.ID, stlist$return <- T, stlist$return <- F)





stlist$hours_1516 <- 0 

stlist[stlist$return == T, "hours_1516"] <- stlist1516[stlist1516$continue == T, "Hours"]

stlist$criteria_1516 <- NA
stlist[stlist$return == T, "criteria_1516"] <- stlist1516[stlist1516$continue == T, "max_criteria"]



write.csv(stlist, "studentlist.csv")



#return <- stlist[stlist$return == T, "Student.ID"]
#continue <- stlist1516[stlist1516$continue == T, "Case.ID"]


#old <- stlist1516[stlist1516$continue == T, "Case.ID"]
 #new <- stlist[stlist$return == T, "Student.ID"]

