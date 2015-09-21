suppressPackageStartupMessages(library(dplyr))
library(car)
#library(afex)

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
  
  # outlier in Customization Mode (one very slow in Control, two with 14 and 18 errors in MC and M)
  data <- subset(data, !(id %in% c("xqpi3r9n")))
  data <- subset(data, !(id %in% c("bicjgan9")))
  data <- subset(data, !(id %in% c("yacy699g")))
  #data <- subset(data, !(id %in% c("0dksgr1h", "c7nb2rhh", "yj20fln6", "8cugv923" )))
  
  # outliers in control
  #data <- subset(data, !(id %in% c("f5arveax", "n30x5hx4", "q7d654an")))
  
  return(data)
}

prepare <- function(data){
  data$id <- factor(data$id)
  
  # rename and reorder interface
  data$interface <- factor(data$interface, c(1,2,3,0), c("Minimal", "Minimal+Context", "Full+Highlight", "Control"))
  
  return(data)
}

sampleControlParticipants <- function(data){
  #selected <- c("35ttjsmh", "0dksgr1h", "pc7v0crm", "qraxf5i3", "sqtqxb7a", "c7nb2rhh", "yj20fln6", "yq1emvsf", "knm8okdb", "vbnkj42r", "8cugv923", "f5arveax");
  #selected <- c("q7d654an", "jhzxk2pr", "3i18spus", "n30x5hx4", "xt4cukka", "0dksgr1h", "pc7v0crm", "vbnkj42r", "yq1emvsf", "yj20fln6", "3e1iqt1k", "f5arveax");
  #selected <- c("tsmka767", "xt4cukka", "yq1emvsf", "3i18spus", "4fyy1fff", "f5arveax", "yj20fln6", "02zz3ylx", "g55seji2", "0dksgr1h", "qraxf5i3", "c7nb2rhh");
  selected <- c("q7d654an", "tsmka767", "yq1emvsf", "4fyy1fff", "n30x5hx4", "3i18spus", "vbnkj42r", "f9gqb0kz", "knm8okdb", "c7nb2rhh", "k7ja666x", "jhzxk2pr");
  return(subset(data, id %in% selected | interface != "Control" ))
}

concatenate <- function(file){
  data <- load.mturk(file)
  data <- removeProblemsAndOutliers(data)
  
  # write the cleaned-up dataframe as a csv file
  write.csv(data, filename("mturk", file), row.names = FALSE, na = "")
}

localMaxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-Inf, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

localMinima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(Inf, x)) < 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

rightMode <- function(data){
  # compute separation point between the two modes
  CAS <- aggregate(correctAnchorHasBeenSelected~id+interface, subset(data, interface!="Control"), mean)
  d <- density(CAS$correctAnchorHasBeenSelected)
  min <- d$x[localMinima(d$y)][2]
  
  writeLines(paste("modes separation point:", round(min, digits=2)))
  
  # find ids of participants in the left mode
  test <- aggregate(correctAnchorHasBeenSelected~id, CAS, mean)
  return(subset(test, correctAnchorHasBeenSelected>min)$id) 
}

