library(actuar)

crmtorg <- function(IndustryIndex=1,
                 Within="Yes",
                 Retention=500000,
                 Deductible=50000,
                 Claims=50,
                 Above="Above Non-Ranking",
                 Iterations=1000,
                 CDF_tmp=CDF,
                 Curr="USD",
                 LoCPerc=85,
                 AggDedPerc=97.5,
                 AccountName="Company ABC"){
  
  ptm <- proc.time()
  xrtoUSD <- Xrates[Xrates[1]==Curr,2]
  RetentionUSD <- Retention * xrtoUSD
  DeductibleUSD <- Deductible * xrtoUSD
  UpperLimit = ifelse(Within == "Yes",RetentionUSD,RetentionUSD+DeductibleUSD)
  if (Above == "Ground Up") GUClaims = Claims else GUClaims = Claims/(1-approx(CDF_tmp[,1],CDF_tmp[,1+IndustryIndex],DeductibleUSD)$y)
  N <- array(0,dim=c(Iterations))
  if (GUClaims < 75) N <- rpois(Iterations,GUClaims) else N <- rnorm(Iterations,GUClaims,sqrt(GUClaims))
  MaxN <- max(N) 
  Losses <- array(0,dim=c(MaxN,Iterations))
  for (i in seq(from=1, to=Iterations, by=1)) {
    if (N[i] > 0) Losses[1:N[i],i] <- approx(CDF_tmp[,1+IndustryIndex],CDF_tmp[,1],runif(N[i]))$y
  }
  LossesInLayer <- pmax((pmin(Losses,UpperLimit) - Deductible),0)
  AggregateLossInLayer <- colSums(LossesInLayer,dim=1)
  MeanLoss <- mean(AggregateLossInLayer)
  LoC <- quantile(x=AggregateLossInLayer,probs=LoCPerc/100,na.rm=TRUE)/MeanLoss
  AggDed <- quantile(x=AggregateLossInLayer,probs=AggDedPerc/100,na.rm=TRUE)/MeanLoss
  Elapsedtime <- proc.time() - ptm
  MeanLossOrig <- MeanLoss/xrtoUSD
  
  
  list(dist=AggregateLossInLayer/xrtoUSD,
       stats=c("Mean LC"=MeanLossOrig,"Ratio LoC to Mean"=LoC[[1]],"LoC"=MeanLossOrig*LoC[[1]],
               "Ratio Agg Ded to Mean"=AggDed[[1]],"Agg Ded"=MeanLossOrig*AggDed[[1]],
               "GU Claims"=GUClaims,"Simulation time"=Elapsedtime[3]),
       inputs=c("IndustryIndex"=IndustryIndex,
                "Within"=Within,
                "Retention"=Retention,
                "Deductible"=Deductible,
                "Claims"=Claims,
                "Above"=Above,
                "Iterations"=Iterations,
                "CDF_tmp"=CDF_tmp,
                "Curr"=Curr,
                "LoCPerc"=LoCPerc,
                "AggDedPerc"=AggDedPerc,
                "AccountName"=AccountName)) 
  
}