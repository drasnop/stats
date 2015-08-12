library(BEST)

# parametric data taken from each participant in the experiment
filename <- function(batch){
  return(paste0("C:/Users/Antoine/programmation/web/stencil-analysis/", batch, "/trials-", batch, ".csv"))
}
csv <- lapply(c("2-24", "3-24", "rect-15"), function(batch) read.csv(filename(batch)))
data <- do.call(rbind, csv)

measure <- "shortDuration"
between <- "interfaceType"

# remove practice trial
data <- subset(data, block>0)

# only first block
# data <- subset(data, block == 1)

#data <- subset(data, (block == 1 & trialNumber >2) | (block == 2 & trialNumber >22))

# remove problematic participants
data <- subset(data, problems <= 0)

# keep only successful trials
#data <- subset(data, success == 1)
data <- subset(data, timeout == 0)
#data <- subset(data, ghost != 1)

# log-transform
data[[measure]] <- log(1+data[[measure]])

writeLines("")
writeLines(paste0("Running BEST for ", measure, " on ", between, " (between-subjects)"));
writeLines("")

# Run the model
y1 <- subset(data, interfaceType == "control")[[measure]]
y2 <- subset(data, interfaceType == "customizationMode")[[measure]]
BESTmodel <- BESTmcmc(y1, y2)

# Print model summary
# check model fit with Rhat < 1.1. If not, increase burnInSteps
# when nu is small, the t distribution has heavy tails; nu > 100: nearly normal
print(BESTmodel)

# Plot 30 random post prediction curves on top of an histogram of the data, to check visually for model fit
plotPostPred(BESTmodel)

# Plot the posterior distribution of the mean difference, with HDI and probabilities
plot(BESTmodel)

