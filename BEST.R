library(BEST)

# parametric data taken from each participant in the experiment
data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/2-24/trials-2-24.csv")
#data <- read.csv("C:/Users/Antoine/programmation/web/stencil-analysis/1-20-pax/trials-1-20-pax.csv")

measure <- "shortDuration"
between <- "interfaceType"
within <- "hookInTutorial"

# remove practice trial
 data <- subset(data, block>0)

# only first block
# data <- subset(data, block == 1)

#data <- subset(data, (block == 1 & trialNumber >2) | (block == 2 & trialNumber >22))

# remove problematic participants
data <- subset(data, problems <= 1)

# keep only successful trials
data <- subset(data, success == 1)
# data <- subset(data, timeout == 0)
#data <- subset(data, ghost != 1)

# log-transform
data[[measure]] <- log(1+data[[measure]])

writeLines("")
writeLines(paste0("Running Parametric Analysis for ", measure, " on ", between, " (between-subjects) and ", within," (within-subject)"));
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

