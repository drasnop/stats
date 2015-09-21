source("util.R")
source("data_manager.R")

# Data for the same likert-style question asked after each block in the experiment
data <- load.lab("intermediate")

measure="speed"
within="interface"

# subset the data
columns <- sapply(0:3, function(x) paste0(measure, x))

util.printBigHeader("Running Non-parametric Within-subjects Analysis")
results <- util.withinSubjectsNonParametricAnalysis(data, columns, participantColumn = "id", dvName = measure, ivName = within, participantName = "id")

# boxplot the data, if it is somewhat numeric
boxplot(as.formula(paste(measure,"~",within)), results$stacked_data)