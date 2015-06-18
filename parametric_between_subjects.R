source("util.R")

# parametric data taken from each participant in the experiment (between-subjects)
all_data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/1-20/durations-participants-1-20.csv")
columns <- c("interface")

# util.printBigHeader("Running Parametric between-subjects Analysis on interface type");
# condition_results <- util.betweenSubjectsAnalysis(all_data, columns, "id", "shortDuration", "interfaceType", "id")

util.printBigHeader("Running Parametric between-subjects Analysis on number of errors");
condition_results <- util.betweenSubjectsAnalysis(all_data, columns, "id", "numSuccesses", "interfaceType", "id")