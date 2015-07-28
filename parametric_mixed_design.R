source("util.R")

# parametric data taken from each participant in the experiment
data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/2-24/trials-2-24.csv")
#data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/2-24/aggregate-2-24.csv")

measure <- "shortDuration"
between <- "interfaceType"
within <- "hookInTutorial"

# remove practice trial
data <- subset(data, block>0)

#data <- subset(data, (block == 1 & trialNumber >2) | (block == 2 & trialNumber >22))

# remove problematic participants
data <- subset(data, problems <=1)

# keep only successful trials
data <- subset(data, success == 1)
# data <- subset(data, timeout == 0)
#data <- subset(data, ghost != 1)

# log-transform
data[[measure]] <- log(1+data[[measure]])

util.printBigHeader(paste0("Running Parametric Analysis for ", measure, " on ", between, " (between-subjects) and ", within," (within-subject)"));
condition_results <- util.mixedDesignAnalysis(data, "id", measure, between, within, "id")
