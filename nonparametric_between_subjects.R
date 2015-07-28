source("util.R")

# Data for the same likert-style question asked after each block in the experiment
data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/2-24/questionnaires-2-24.csv")

dvName <- "liking"

util.printBigHeader("Running Non-parametric Within-subjects Analysis")
likert_results <- util.betweenSubjectsNonParametricAnalysis(data, c(dvName), "id",  dvName, ivName="interfaceType", participantName = "id")