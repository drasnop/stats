source("util.R")

# parametric data taken from each participant in the experiment (between-subjects)
data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/2-24/questionnaires-2-24.csv")

util.printBigHeader("Running Parametric between-subjects Analysis");
condition_results <- util.betweenSubjectsAnalysis(data, "id", "tabsRecognitionScore", "interface", "id")