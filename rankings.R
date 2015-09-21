library("pmr")
source("data_manager.R")

# Data for the rankings provided at the end of the experiment
data <- load.lab("rankings")

measure="likingRank"
within="interface"

# subset the data
columns <- sapply(0:3, function(x) paste0(measure, x))

util.printBigHeader(paste("Running Non-parametric Analysis on",measure,"by",within))

# compute freq of first rank
means <- sapply(1:4, function(x) mean(data[[columns[x]]]))
freqs1 <- sapply(1:4, function(x) sum(data[[columns[x]]] == 1))
freqs1type <- c(freqs[1], sum(freqs[2], freqs[3], freqs[4]))

# chi square test vs uniform distribution
print(chisq.test(freqs1), simulate.p.value = TRUE, B=10000)
print(fisher.test(rbind(freqs1, c(3,3,3,3))))

# compute freqs for each rank
freqs <- sapply(1:4, function(x){
  sapply(1:4, function(y) sum(data[[columns[y]]] == x))
})
print(freqs)

# chi square test of independence
print(chisq.test(freqs), simulate.p.value = TRUE, B=10000)
print(fisher.test(freqs))
