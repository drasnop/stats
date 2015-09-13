n = 1000;

results <- lapply(1:n, function(x) {
  ids <<- sample(as.character(subset(collapsed, interface == "Control" & block == "1")$id), 12)
  sampled <<- subset(collapsed, id %in% ids | interface != "Control")

  suppressWarnings(result <- eval(parse(text=paste0("ezANOVA(data=sampled, dv=", measure, ", wid=id", ", between=", between, ", within=", within, ", type=3)"))))
  print(paste(x, result$ANOVA$p[1]))
  
  return(result$ANOVA);
})

# print one summary of the data
print(eval(parse(text=paste0("ezStats(data=sampled, dv=", measure, ", wid=id", ", between=", between, ", within=", within, ")"))))

# print distributions of interesting parameters
distribution <- function(parameter, index){
  values <- sapply(results, function(ANOVA){
    return(ANOVA[[parameter]][[index]])
  })
  
  plot(density(values), main=paste0(parameter, '[', index, ']'))
  #hist(values, breaks=50, main=paste0(parameter, '[', index, ']'))
}

distribution("p", 1)
distribution("ges", 1)


#print(paste(length(ids), length(subset(sampled, interface == "Control" & block == "1")$id)))

#temp <- split(collapsed, collapsed$interface)
#control <- split(temp$`0`, temp$`0`$block)
#a <- sample_n(control$`1`, 8)
#b <- sample_n(control$`2`, 8)
#sampled <- rbind(a, b)
#merged <- rbind(sampled, temp$`1`, temp$`2`, temp$`3`)