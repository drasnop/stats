source("util.R")
source("data_manager.R")

data <- load.mturk("questionnaires")
data <- prepare(data)
data <- removeProblemsAndOutliers(data)
data <- sampleControlParticipants(data)

measure <- "tabsRecognitionScore"
between <- "interface"

# log-transform
if(is.numeric(data[[measure]]))
  data[[measure]] <- log(1+data[[measure]])

# make sure factors are treated as factors
data[[between]] <- factor(data[[between]])

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

# de-logtransform effect sizes
delog <- function(x) exp(x)-1
difference <- function(x, y){
  absolute <- delog(x)-delog(y)
  percentage <- absolute/delog(x)
  writeLines(paste0(round(absolute, digits=2), ' ', round(percentage*100, digits=0), '%'))  
} 
means.df <- data %>% group_by(interface)  %>% summarize(Mean=mean(tabsRecognitionScore))
means <- list()
by(means.df, 1:nrow(means.df), function(row) means[as.character(row$interface)] <<- row$Mean)
difference(means$Full, means$Minimal)
difference(means$Control, means$Minimal)