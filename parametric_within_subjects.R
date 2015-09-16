source("util.R")
source("data_manager.R")

# parametric data taken from each participant in the experiment (within-subjects)
data <- load.lab("trials")

measure <- "shortDuration"
estimator <- median
within <- "interface"

# remove practice trial
data <- subset(data, block!=0)

# keep only successful trials
#data <- subset(data, success)
#data <- subset(data, !timeout)
#data <- subset(data, targetGhost != 1)

#data <- subset(data, targetTab != "Shortcuts")

# log-transform
if(is.numeric(data[[measure]]))
  data[[measure]] <- log(1+data[[measure]])

# make sure factors are treated as factors
data[[within]] <- factor(data[[within]])
data$block <- factor(data$block)

# rename and reorder interface
data$interface <- factor(data$interface, c(1,2,3,0), c("Minimal", "Minimal+Context", "Full", "Control"))

if(is.numeric(data[[measure]])){
  # plot histograms to check normality
  par(mfrow=c(2,2))
  aggregate(as.formula(paste(measure,"~",within)), data, function(x) hist(x, breaks=30, main=paste(measure,"~",within)) )
  
  # plot kernel density plots to check normality
  aggregate(as.formula(paste(measure,"~",within)), data, function(x) plot(density(x), main=paste(measure,"~",within)) )
  par(mfrow=c(1,1))
}


## aggregate
collapsed <- aggregate(as.formula(paste(measure,"~ id +",within)), data, estimator)
util.printBigHeader(paste0("Running Parametric Analysis for ", measure, " on ", within," (within-subject)"));


## Assumptions

# plot histograms to check normality
hist(collapsed[[measure]], breaks=30)

# plot kernel density plots to check normality
plot(density(collapsed[[measure]]))


## ANOVA
results <- util.withinSubjectsAnalysis(collapsed, "id", measure, within, "id")

# boxplot the data to look for outliers in each cell
outliers <- boxplot(as.formula(paste(measure,"~",within)), collapsed)$out

# print outliers
if(length(outliers) > 0){
  util.printHeader("Outliers")
  print(collapsed[collapsed[[measure]] %in% outliers, ])
}

# de-logtransform effect sizes
delog <- function(x) exp(x)-1
difference <- function(x, y){
  absolute <- delog(x)-delog(y)
  percentage <- absolute/delog(x)
  writeLines(paste0(round(absolute, digits=2), ' ', round(percentage*100, digits=0), '%'))  
} 
means.df <- data %>% group_by(interface)  %>% summarize(Mean=mean(shortDuration))
means <- list()
by(means.df, 1:nrow(means.df), function(row) means[as.character(row$interface)] <<- row$Mean)
difference(means$Full, means$`Minimal+Context`)
difference(means$Control, means$`Minimal+Context`)