source("util.R")
source("data_manager.R")

data <- load.mturk("questionnaires")
data <- removeProblemsAndOutliers(data)

util.printBigHeader("Running Parametric between-subjects Analysis");
condition_results <- util.betweenSubjectsAnalysis(data, "id", "tabsRecognitionScore", "interface", "id")