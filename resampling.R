n = 100;

pvalues <- sapply(1:n, function(x) {
  ids <- sample(subset(collapsed, interface == "0")$id, 12)
  sampled <- subset(collapsed, id %in% ids | interface != "0")
  print(paste(length(ids), length(subset(sampled, interface == "0"))))
  
  suppressWarnings(results <- eval(parse(text=paste0("ezANOVA(data=sampled, dv=", measure, ", wid=id", ", between=", between, ", within=", within, ", type=3)"))))
  return(results$ANOVA$p[1]);
})

# one summary of the data
print(eval(parse(text=paste0("ezStats(data=sampled, dv=", measure, ", wid=id", ", between=", between, ", within=", within, ")"))))

plot(density(pvalues))
hist(pvalues, breaks=30)

#temp <- split(collapsed, collapsed$interface)
#control <- split(temp$`0`, temp$`0`$block)
#a <- sample_n(control$`1`, 8)
#b <- sample_n(control$`2`, 8)
#sampled <- rbind(a, b)
#merged <- rbind(sampled, temp$`1`, temp$`2`, temp$`3`)