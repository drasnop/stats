source("util.R")

# Data for the same likert-style question asked after each block in the experiment
all_data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/lab/intermediate-lab.csv")
#all_data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/lab/rankings-lab.csv")

measure="liking"
within="interface"

# subset the data
columns <- sapply(0:3, function(x) paste0(measure, x))
data <- util.withinSubjectsStack(all_data, columns, participantColumn = "id", dvName = measure, ivName = within, participantName = "id")$stack

util.printBigHeader(paste("Running Parametric Analysis for", measure, "on", within,"(within-subject)"));
results <- util.withinSubjectsAnalysis(data, participantColumn = "id", dvName = measure, ivName = within, participantName = "id")

# boxplot the data, if it is somewhat numeric
boxplot(as.formula(paste(measure,"~",within)), results$stacked_data)