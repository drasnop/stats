source("util.R")
#suppressPackageStartupMessages(library(dplyr))

# parametric data taken from each participant in the experiment
filename <- function(batch){
  return(paste0("C:/Users/Antoine/programmation/web/stencil-analysis/", batch, "/trials-", batch, ".csv"))
}
csv <- lapply(c("2-24", "3-24", "rect-15"), function(batch) read.csv(filename(batch)))
data <- do.call(rbind, csv)

measure <- "shortDuration"
estimator <- median
between <- "partition"
within <- "block"

data$timeout <- as.logical(data$timeout)
data$success <- as.logical(data$success)

# remove practice trial
data <- subset(data, block!=0)

# remove problematic participants
data <- subset(data, problems<=0)

# outlier in Customization Mode
data <- subset(data, !(id %in% c("xqpi3r9n")))
#data <- subset(data, !(id %in% c("0dksgr1h", "c7nb2rhh", "yj20fln6", "8cugv923" )))

# outliers in control
data <- subset(data, !(id %in% c("f5arveax", "n30x5hx4", "q7d654an")))

# keep only successful trials
#data <- subset(data, success)
#data <- subset(data, !timeout)
#data <- subset(data, targetGhost != 1)


# log-transform
#if(estimator == mean)
if(is.numeric(data[[measure]]))
  data[[measure]] <- log(1+data[[measure]])

# make sure factors are treated as factors
data[[within]] <- factor(data[[within]])
data[[between]] <- factor(data[[between]])

if(is.numeric(data[[measure]])){
  # plot histograms to check normality
  par(mfrow=c(nlevels(data[[within]]),nlevels(data[[between]])))
  aggregate(as.formula(paste(measure,"~",within,"+",between)), data, function(x) hist(x, breaks=30, main=paste(measure,"~",between)) )
  
  # plot kernel density plots to check normality
  aggregate(as.formula(paste(measure,"~",within,"+",between)), data, function(x) plot(density(x), main=paste(measure,"~",between)) )
  par(mfrow=c(1,1))
}

  
# aggregate
collapsed <- aggregate(as.formula(paste(measure,"~ id +",within,"+",between)), data, estimator)

# plot histograms to check normality
hist(collapsed[[measure]], breaks=30)

# plot kernel density plots to check normality
plot(density(collapsed[[measure]]))



# ANOVA
util.printBigHeader(paste0("Running Parametric Analysis for ", measure, " on ", between, " (between-subjects) and ", within," (within-subject)"));
results <- util.mixedDesignAnalysis(collapsed, "id", measure, between, within, "id")


# boxplot the data to look for outliers in each cell
#outliers <- boxplot(as.formula(paste(measure,"~",within,"+",between)), collapsed)$out
outliers <- boxplot(as.formula(paste(measure,"~",between,"+",within)), collapsed)$out

# print outliers
if(length(outliers) > 0){
  util.printHeader("Outliers")
  print(collapsed[collapsed[[measure]] %in% outliers, ])
}

# boxplot main effect
boxplot(as.formula(paste(measure,"~",between)), collapsed)
