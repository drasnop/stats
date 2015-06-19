source("util.R")

# parametric data taken from each participant in the experiment
all_data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/1-20-pax/trials-1-20-pax.csv")
columns <- c("interface") # meh

util.printBigHeader("Running Parametric Analysis on interface type (between-subjects) and trial number (within-subject)");
condition_results <- util.mixedDesignAnalysis(all_data, columns, "id", "shortDuration", "interfaceType", "trialNumber", "id")