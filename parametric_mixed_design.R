source("util.R")

# parametric data taken from each participant in the experiment
filename <- function(batch){
  return(paste0("C:/Users/Antoine/programmation/web/stencil-analysis/", batch, "/trials-", batch, ".csv"))
}
csv <- lapply(c("2-24", "3-24", "rect-15"), function(batch) read.csv(filename(batch)))
data <- do.call(rbind, csv)

measure <- "shortDuration"
estimator <- median
between <- "interface"
within <- "block"

# remove practice trial
data <- subset(data, block!=0)

# remove problematic participants
data <- subset(data, problems<=0)

# remove outliers
#data <- subset(data, !(id %in% c("f5arveax")))
#data <- subset(data, !(id %in% c("sgcg1k70")))
#data <- subset(data, !(id %in% c("f5arveax", "n30x5hx4", "q7d654an")))

# keep only successful trials
#data <- subset(data, success == 1)
data <- subset(data, timeout == 0)
#data <- subset(data, targetGhost != 1)


# log-transform
#if(estimator == mean)
data[[measure]] <- log(1+data[[measure]])

# make sure factors are treated as factors
data[[within]] <- factor(data[[within]])
data[[between]] <- factor(data[[between]])


# plot histograms to check normality
par(mfrow=c(nlevels(data[[within]]),nlevels(data[[between]])))
aggregate(as.formula(paste(measure,"~",within,"+",between)), data, function(x) hist(x, breaks=30, main=paste(measure,"~",between)) )

# plot kernel density plots to check normality
aggregate(as.formula(paste(measure,"~",within,"+",between)), data, function(x) plot(density(x), main=paste(measure,"~",between)) )
par(mfrow=c(1,1))

  
# aggregate
collapsed <- aggregate(as.formula(paste(measure,"~ id +",within,"+",between)), data, estimator)

# ANOVA
util.printBigHeader(paste0("Running Parametric Analysis for ", measure, " on ", between, " (between-subjects) and ", within," (within-subject)"));
condition_results <- util.mixedDesignAnalysis(collapsed, "id", measure, between, within, "id")

# post-hoc tests
#print(pairwise.t.test(collapsed[[measure]], collapsed[[between]], p.adjust.method="bonferroni", pool.sd=F))
#print(pairwise.t.test(collapsed[[measure]], collapsed[[within]], p.adjust.method="bonferroni", paired=T))

# boxplot the data
#outliers <- boxplot(as.formula(paste(measure,"~",within,"+",between)), collapsed)$out
outliers <- boxplot(as.formula(paste(measure,"~",between,"+",within)), collapsed)$out

# print outliers
util.printHeader("Outliers")
print(collapsed[collapsed[[measure]] %in% outliers, ])

# UNUSED
#data <- subset(data, (block == 1 & trialNumber >2) | (block == 2 & trialNumber >22))
