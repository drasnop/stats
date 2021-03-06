source("util.R")
source("data_manager.R")

# Data for likert-style questions asked at the end of the experiment
data <- load.mturk("questionnaires")
data <- prepare(data)
data <- removeProblemsAndOutliers(data)
data <- sampleControlParticipants(data)

measure <- "easeOfUse"
between <- "interface"

util.printBigHeader("Running Non-parametric Within-subjects Analysis")
likert_results <- util.betweenSubjectsNonParametricAnalysis(data, c(measure), "id",  dvName=measure, ivName=between, participantName = "id")
