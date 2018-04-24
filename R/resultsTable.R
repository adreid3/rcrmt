resultsTable <- function (datasetInput) {
  results <- try(as.character(datasetInput[[2]]))
  if(class(results) == "try-error") results <- c("","","","","","","") else
  {
    rounddecimals <- c(0,2,0,2,0,0,3)
    roundedvalues <- round(datasetInput[[2]],rounddecimals)
    for (i in 1:length(rounddecimals)) {
      results[i] <- as.character(format(roundedvalues[i],big.mark=",",scientific=F))
    }
  }
  data.frame(
    Quantity = c(
      "Mean Loss Cost",
      "Ratio of Letter of Credit (LoC) Required to the Mean", 
      "Required LoC",
      "Ratio of Aggregate Deductible to the Mean",
      "Aggregate Deductible",
      "Expected Number of Claims (Ground up)",
      "Simulation run time (s)"),
    Value = results, 
    stringsAsFactors=FALSE)
}