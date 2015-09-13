source("util.R")
source("data_manager.R")

data <- load.mturk("questionnaires")
data <- removeProblemsAndOutliers(data)

measure <- "tabsRecognitionScore"
between <- "interface"

# log-transform
if(is.numeric(data[[measure]]))
  data[[measure]] <- log(1+data[[measure]])

# make sure factors are treated as factors
data[[between]] <- factor(data[[between]])

# rename and reorder interface
data$interface <- factor(data$interface, c(1,2,3,0), c("Minimal", "Minimal+Context", "Full", "Control"))

if(is.numeric(data[[measure]])){
  # plot histograms to check normality
  par(mfrow=c(nlevels(data[[between]])/2,nlevels(data[[between]])/2))
  aggregate(as.formula(paste(measure,"~",between)), data, function(x) hist(x, breaks=30, main=paste(measure,"~",between)) )
  
  # plot kernel density plots to check normality
  aggregate(as.formula(paste(measure,"~",between)), data, function(x) plot(density(x), main=paste(measure,"~",between)) )
  par(mfrow=c(1,1))
}

util.printBigHeader("Running Parametric between-subjects Analysis");
condition_results <- util.betweenSubjectsAnalysis(data, "id", measure, between, "id")