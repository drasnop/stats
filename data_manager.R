filename <- function(batch, file){
  return(paste0("C:/Users/Antoine/programmation/web/stencil-analysis/", batch, "/", file, "-", batch, ".csv"))
}

load.mturk <- function(file){
  csv <- lapply(c("2-24", "3-24", "rect-15", "4-12", "rect-2"), function(batch) read.csv(filename(batch, file)))
  return(do.call(rbind, csv))  
}

load.lab <- function(file){
  return(read.csv(filename("lab", file)))
}

removeProblemsAndOutliers <- function(data){
  # remove problematic participants
  data <- subset(data, problems<=0)
  
  # outlier in Customization Mode (one very slow, one with 14 errors, one with 18 errors)
  data <- subset(data, !(id %in% c("xqpi3r9n")))
  data <- subset(data, !(id %in% c("bicjgan9")))
  data <- subset(data, !(id %in% c("yacy699g")))
  #data <- subset(data, !(id %in% c("0dksgr1h", "c7nb2rhh", "yj20fln6", "8cugv923" )))
  
  # outliers in control
  #data <- subset(data, !(id %in% c("f5arveax", "n30x5hx4", "q7d654an")))
  
  return(data)
}

concatenate <- function(file){
  data <- load.mturk(file)
  data <- removeProblemsAndOutliers(data)
  
  # write the cleaned-up dataframe as a csv file
  write.csv(data, filename("mturk", file), row.names = FALSE, na = "")
}


