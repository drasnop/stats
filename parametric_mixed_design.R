source("util.R")

# parametric data taken from each participant in the experiment
# data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/2-24/trials-2-24.csv")
data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/2-24/aggregate-2-24.csv")

# remove practice trial
data <- subset(data, block>0)

# remove problematic participants
data <- subset(data, problems <=1)

# keep only successful trials
#data <- subset(data, success == 1)
# data <- subset(data, timeout == 0)

# log-transform
#data$shortDuration <- log(data$shortDuration)

util.printBigHeader("Running Parametric Analysis on interface type (between-subjects) and block (within-subject)");
condition_results <- util.mixedDesignAnalysis(data, "id", "medianShortDuration", "interfaceType", "block", "id")
