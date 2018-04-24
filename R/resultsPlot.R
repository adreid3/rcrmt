# put in a comment

resultsPlot <- function (datasetInput) {
  results <- try(as.character(datasetInput[[2]]))
  if(class(results) != "try-error") {  
    dist <- datasetInput$dist
    stats <- datasetInput$stats
    inputs <- try(datasetInput$inputs)
    #if (class(inputs) == "try-error") {} else {
    inputs <- datasetInput$inputs
    myhist <- hist(dist,xlim=c(0,max(dist)),cex.lab=1.2,cex.main=1.5,col=gray(0.8),breaks=50,main=paste("Aggregate Loss Distribution for ",inputs$AccountName,sep=""),xlab=paste("Aggregate Loss (",inputs$Curr,")",sep=""))    
    mydensity <- density(dist)
    multiplier <- myhist$counts / myhist$density
    mydensity$y <- mydensity$y * multiplier[1]
    lines(mydensity,lwd=2) 
    if (dist[1] > 0) {
      if (inputs$Within == "Yes") limit <- inputs$Retention else limit <- inputs$Retention + inputs$Deductible
      abline(v=limit,col="black",lwd=4)
      abline(v=inputs$Deductible,col="black",lwd=4)
      abline(v=stats[1],col="red",lwd=4)
      abline(v=stats[3],col="blue",lwd=4)
      abline(v=stats[5],col="green",lwd=4)
      text(x=inputs$Deductible,y=max(myhist$counts),labels=paste("Deductible (",format(inputs$Deductible,big.mark=",",scientific=F),")",sep=""),adj=c(-0.1,0))
      text(x=limit,y=max(myhist$counts),labels=paste("Top of Layer (",format(limit,big.mark=",",scientific=F),")",sep=""),adj=c(-0.1,2))
      text(x=stats[1],y=max(myhist$counts),labels=paste("Mean LC (",format(round(stats[1],0),big.mark=",",scientific=F),")",sep=""),adj=c(-0.1,4))
      text(x=stats[3],y=max(myhist$counts),labels=paste("85th LC (",format(round(stats[3],0),big.mark=",",scientific=F),")",sep=""),adj=c(-0.1, 8))
      text(x=stats[5],y=max(myhist$counts),labels=paste("Agg Ded (",format(round(stats[5],0),big.mark=",",scientific=F),")",sep=""),adj=c(-0.1, 10))
    }
    #}
  } else
  {
    #histdata <- c(0,0)
    #linedata <- c(0,0)
    #hist(histdata,xlim=c(0,10000000),main=paste("Aggregate Loss Distribution for",inputs$AccountName),xlab=paste("Aggregate Loss (",inputs$Curr,")"))  
  }
}