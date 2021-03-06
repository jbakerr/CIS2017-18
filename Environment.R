#install.packages("dplyr") # These packages need to be installed once, after that they can be called up using library. This package is for restructuring data
#install.packages("XLConnect") # This package is for loading / writing excel spreadsheets
#install.packages("tidyr")
#install.packages("plyr")
#install.packages("data.table")
#install.packages("lubridate")
#instal.packages("xlsx")
#options(java.parameters = "-Xmx4g" )
# install.packages('openxlsx')
# install.packages('readxl')
# install.packages("devtools")
# install.packages("shiny")
# install.packages("googlesheets")
# install.packages('googledrive')
options(java.parameters = "-Xmx3000m")


library(plyr) # ****IMPORTANT**** Load plyr before dplyr- they have some of the same named functions, and if you load in a different order it will cause problems
library(dplyr)
library(tidyr)
library(XLConnect)
# library(data.table)
library(lubridate)
# library(openxlsx)
# library(xlsx)
# library(readxl)
library(shiny)
# library(openxlsx)
# library(googlesheets)
# library(googledrive)
library(readxl)


###################################      Generating Student List      ##########################################
# Set the working directory to the local folder containing dataset. Can be done manually
# by going to Session -> Set Working Directory -> Choose Directory 
macdatawd <- "/Volumes/GoogleDrive/Team Drives/Data/CISDM Files/"
windowsdatawd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}



#Setting Vectors that will be used throughout program
metrics <- c("Math","Science","ELA", "Suspensions", "Attendance Rate")

elem <- c("Glenn Elementary School", "Eno Valley Elementary", "EK Powe Elementary School", "Merrick-Moore")
high <- c("Shepard", "Durham Performance Learning Center", "Hillside High School", "Southern High School", "Northern")


