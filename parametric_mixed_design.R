source("util.R")
suppressPackageStartupMessages(library(dplyr))
#library(car)
#library(afex)

# parametric data taken from each participant in the experiment
filename <- function(batch){
  return(paste0("C:/Users/Antoine/programmation/web/stencil-analysis/", batch, "/trials-", batch, ".csv"))
}
csv <- lapply(c("2-24", "3-24", "rect-15", "4-12", "rect-2"), function(batch) read.csv(filename(batch)))
data <- do.call(rbind, csv)

measure <- "shortDuration"
estimator <- median
between <- "interface"
within <- "block"

# remove practice trial
data <- subset(data, block!=0)

# remove problematic participants
data <- subset(data, problems<=0)

# outlier in Customization Mode (one very slow, one with 14 errors, one with 18 errors)
data <- subset(data, !(id %in% c("xqpi3r9n")))
data <- subset(data, !(id %in% c("bicjgan9")))
data <- subset(data, !(id %in% c("yacy699g")))
#data <- subset(data, !(id %in% c("0dksgr1h", "c7nb2rhh", "yj20fln6", "8cugv923" )))

# outliers in control
#data <- subset(data, !(id %in% c("f5arveax", "n30x5hx4", "q7d654an")))

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