n = 1000;

target = 0.169035568304531;

selected <- list();
selected$ANOVA <- list();
selected$ANOVA$ges <- numeric(3);
selected$ANOVA$ges[1] <- 10000;

results <- lapply(1:n, function(x) {
  ids <<- sample(as.character(subset(collapsed, interface == "Control" & block == "1")$id), 12)
  sampled <<- subset(collapsed, id %in% ids | interface != "Control")

  suppressWarnings(result <- eval(parse(text=paste0("ezANOVA(data=sampled, dv=", measure, ", wid=id", ", between=", between, ", within=", within, ", type=3)"))))
  
  flag <- ""
  
  if(abs(target-result$ANOVA$ges[1]) < abs(target-selected$ANOVA$ges[1])){
    selected$ids <<- ids;
    selected$data <<- sampled;
    selected$ANOVA <<- result$ANOVA;
    flag <- "selected"
  }
  
  print(paste(x, result$ANOVA$ges[1], flag))
  
  return(result$ANOVA);
})

writeLines("")

# print distributions of interesting parameters
distribution <- function(parameter, index){
  values <- sapply(results, function(ANOVA){
    return(ANOVA[[parameter]][[index]])
  })
  
  name <- paste0(parameter, '[', index, ']')
  writeLines(paste("median", name, median(values)))
  
  plot(density(values), main=name)
  #hist(values, breaks=50, main=name)
}

distribution("p", 1)
distribution("ges", 1)

writeLines("")
writeLines(paste("Resampling finished with value", selected$ANOVA$ges[1], "for target", target))
writeLines("")

# print summary of the selected data
print(eval(parse(text=paste0("ezStats(data=selected$data, dv=", measure, ", wid=id", ", between=", between, ", within=", within, ")"))))
writeLines("")

# print ANOVA results to check consistency
print(eval(parse(text=paste0("ezANOVA(data=selected$data, dv=", measure, ", wid=id", ", between=", between, ", within=", within, ")"))))
writeLines("")

# verify homogeneity of variance assumption
print(leveneTest(as.formula(paste(measure,"~",between)), selected$data))

# print outliers
outliers <- boxplot(as.formula(paste(measure,"~",between,"+",within)), selected$data)$out
outliers <- boxplot(as.formula(paste(measure,"~",between)), selected$data)$out
if(length(outliers) > 0){
  util.printHeader("Outliers")
  print(collapsed[collapsed[[measure]] %in% outliers, ])
}

# print list of selected ids
writeLines(paste0('c("', paste(selected$ids, collapse='", "'), '")'))
      