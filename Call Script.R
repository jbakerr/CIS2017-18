#Run Script to Call All Scripts

macwd <- "/Users/baker/Code/CIS2017-18"
windowswd <- "C:/Users/USER/Code/CIS-2016-17"
dir <- FALSE

dir <- ifelse(file.exists(macwd), dir <- TRUE, dir <- FALSE)

if(file.exists(macwd)){
  setwd(file.path(macwd))
} else { 
  if(file.exists(windowswd)){
    setwd(file.path(windowswd))
  }
}


source("Environment.R")

if(file.exists(macwd)){
  setwd(file.path(macwd))
} else { 
  if(file.exists(windowswd)){
    setwd(file.path(windowswd))
  }
}

source("Cleaning Metrics.R")

if(file.exists(macwd)){
  setwd(file.path(macwd))
} else { 
  if(file.exists(windowswd)){
    setwd(file.path(windowswd))
  }
}

source("Cleaning Services.R")


if(file.exists(macwd)){
  setwd(file.path(macwd))
} else { 
  if(file.exists(windowswd)){
    setwd(file.path(windowswd))
  }
}

#source("case_management.R")


if(file.exists(macwd)){
  setwd(file.path(macwd))
} else { 
  if(file.exists(windowswd)){
    setwd(file.path(windowswd))
  }
}


source("Student Metrics.R")


if(file.exists(macwd)){
  setwd(file.path(macwd))
} else { 
  if(file.exists(windowswd)){
    setwd(file.path(windowswd))
  }
}

#source("Returning Students.R")

#END OF SCRIPT





