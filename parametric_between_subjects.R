source("util.R")

# parametric data taken from each participant in the experiment (between-subjects)
all_data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/2-24/aggregate-2-24.csv")

util.printBigHeader("Running Parametric between-subjects Analysis on interface type");
condition_results <- util.betweenSubjectsAnalysis(all_data, "id", "medianShortDuration", "interfaceType", "id")

# util.printBigHeader("Running Parametric between-subjects Analysis on number of errors");
# condition_results <- util.betweenSubjectsAnalysis(all_data, "id", "numSuccesses", "interfaceType", "id")