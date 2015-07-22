source("util.R")

# parametric data taken from each participant in the experiment (between-subjects)
data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/2-24/trials-2-24.csv")
#data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/2-24/aggregate-2-24.csv")

# remove practice trial
data <- subset(data, block>0)

# remove participants with major problems
data <- subset(data, problems <=1)

# keep only successful trials
data <- subset(data, success == 1)
# data <- subset(data, timeout == 0)

# log-transform
data$shortDuration <- log(data$shortDuration)

util.printBigHeader("Running Parametric between-subjects Analysis on interface type");
condition_results <- util.betweenSubjectsAnalysis(data, "id", "shortDuration", "interfaceType", "id")

# util.printBigHeader("Running Parametric between-subjects Analysis on number of errors");
# condition_results <- util.betweenSubjectsAnalysis(data, "id", "numSuccesses", "interfaceType", "id")