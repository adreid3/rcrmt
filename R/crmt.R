# This is to test the local git repo is working ok.

library(actuar)

library(foreach)

crmt <- function(IndustryIndex=1,
                    Within="Yes",
                    Retention=500000,
                    Deductible=10000,
                    Claims=40,
                    Above="Ground Up",
                    Iterations=5000,
                    CDF_tmp=CDF1,
                    Curr="USD",
                    LoCPerc=85,
                    AggDedPerc=97.5,
                    AccountName="Company ABC"){
   
#   IndustryIndex<-1
#   Within<-"Yes"
#   Retention<-500000
#   Deductible<-10000
#   Claims<-40
#   Above<-"Ground Up"
#   Iterations<-5000
#   CDF_tmp<-CDF1
#   Curr<-"USD"
#   LoCPerc<-85
#   AggDedPerc<-97.5
#   AccountName<-"Company ABC"
  
  ptm <- proc.time()
  xrtoUSD <- Xrates[Xrates[1]==Curr,2]
  RetentionUSD <- Retention * xrtoUSD
  DeductibleUSD <- Deductible * xrtoUSD
  UpperLimit = ifelse(Within == "Yes",RetentionUSD,RetentionUSD+DeductibleUSD)
  if (Above == "Ground Up") GUClaims = Claims else GUClaims = Claims/(1-approx(CDF_tmp[,1],CDF_tmp[,1+IndustryIndex],DeductibleUSD)$y)
  #if (Above == "Ground Up") ExcessClaims = Claims*(1-approx(CDF_tmp[,1],CDF_tmp[,1+IndustryIndex],DeductibleUSD)$y) else ExcessClaims = Claims
  N <- array(0,dim=c(Iterations))
  if (GUClaims < 75) N <- rpois(Iterations,GUClaims) else N <- rnorm(Iterations,GUClaims,sqrt(GUClaims))
  #if (ExcessClaims < 75) N <- rpois(Iterations,ExcessClaims) else N <- rnorm(Iterations,ExcessClaims,sqrt(ExcessClaims))
  MaxN <- max(N) 
  #Losses <- array(0,dim=c(MaxN,Iterations))
  Losses <- array(approx(CDF_tmp[,1+IndustryIndex],CDF_tmp[,1],runif(MaxN*Iterations))$y,dim=c(MaxN,Iterations))
  for (i in seq(from=1, to=Iterations, by=1)) {
    #if (N[i] > 0) Losses[1:N[i],i] <- approx(CDF_tmp[,1+IndustryIndex],CDF_tmp[,1],runif(N[i]))$y
    if (N[i] < MaxN) Losses[(N[i]+1):MaxN,i] <- 0
  }
  #foreach (i=1:Iterations, .combine=cbind) %do% if (N[i] > 0) Losses[1:N[i],i] <- approx(CDF_tmp[,1+IndustryIndex],CDF_tmp[,1],runif(N[i]))$y 
  #Losses <- foreach (i=1:1000, .combine=cbind) %:% foreach (j=1:2) %do% LossAmt() 
  LossesInLayer <- pmax((pmin(Losses,UpperLimit) - Deductible),0)
  #LossesInLayer <- pmax((pmin(Losses,UpperLimit)),0)
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