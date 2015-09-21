source("util.R")
source("data_manager.R")

# parametric data taken from each participant in the experiment
data <- load.mturk("trials")
data <- prepare(data)
data <- removeProblemsAndOutliers(data)
data <- sampleControlParticipants(data)
nParticipants <- length(unique(data$id))
anchorSearch.ids <- rightMode(data)

measure <- "shortDuration"
estimator <- median
between <- "interface"
within <- "block"

# remove "anchor search" or "search full panel" participants
data <- subset(data, (id %in% anchorSearch.ids) | interface=="Control")
data <- subset(data, interface!="Full+Highlight")

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
data[[between]] <- factor(data[[between]])


if(is.numeric(data[[measure]])){
  # plot histograms to check normality
  par(mfrow=c(nlevels(data[[within]]),nlevels(data[[between]])))
  aggregate(as.formula(paste(measure,"~",within,"+",between)), data, function(x) hist(x, breaks=30, main=paste(measure,"~",between)) )
  
  # plot kernel density plots to check normality
  aggregate(as.formula(paste(measure,"~",within,"+",between)), data, function(x) plot(density(x), main=paste(measure,"~",between)) )
  par(mfrow=c(1,1))
}

  
## aggregate
collapsed <- aggregate(as.formula(paste(measure,"~ id +",within,"+",between)), data, estimator)
util.printBigHeader(paste0("Running Parametric Analysis for ", measure, " on ", between, " (between-subjects) and ", within," (within-subject)"));


## Assumptions

# plot histograms to check normality
hist(collapsed[[measure]], breaks=30)

# plot kernel density plots to check normality
plot(density(collapsed[[measure]]))

# Homogeneity of variance test
print(leveneTest(as.formula(paste(measure,"~",between)), collapsed))
# non-parametric
#fligner.test(as.formula(paste(measure,"~interaction(",within,",",between,")")), collapsed)

# no need to test for sphericity if only two levels of within-subject factor


## ANOVA
results <- util.mixedDesignAnalysis(collapsed, "id", measure, between, within, "id")
# model <- mixed(shortDuration ~ interface + block + interface:block + (1|id), collapsed) using package afex

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

# de-logtransform effect sizes
writeLines("")
delog <- function(x) exp(x)-1
difference <- function(x, y){
  absolute <- delog(x)-delog(y)
  percentage <- absolute/delog(x)
  writeLines(paste0(round(absolute, digits=2), ' ', round(percentage*100, digits=0), '%'))  
} 
means.df <- data %>% group_by(interface)  %>% summarize(Mean=mean(shortDuration))
means <- list()
by(means.df, 1:nrow(means.df), function(row) means[as.character(row$interface)] <<- row$Mean)
#difference(means$Full, means$Minimal)
difference(means$Control, means$Minimal)

writeLines(paste((nParticipants-length(unique(data$id))), "participants removed out of", nParticipants))

# verify that design in balanced
#data %>% group_by(interface, partition, defaults, block) %>% summarize(count=n()/20) %>% print.data.frame()
