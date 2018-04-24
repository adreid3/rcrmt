\name{crmt}
\alias{crmt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CRMT
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
crmt(IndustryIndex, Within, Retention, Deductible, Claims, Above, Iterations, CDF_tmp, Curr, LoCPerc, AggDedPerc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{IndustryIndex}{
Identifies which ILF curve to use.
}
  \item{Within}{
="Yes"
}
  \item{Retention}{
=500'000
}
  \item{Deductible}{
=50'000
}
  \item{Claims}{
=50
}
  \item{Above}{
="Above Non-Ranking"
}
  \item{Iterations}{
=1000
}
  \item{CDF_tmp}{
=CDF
}
  \item{Curr}{
="USD"
}
  \item{LoCPerc}{
=85
}
  \item{AggDedPerc}{
=97.5
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (IndustryIndex, Within, Retention, Deductible, Claims, 
    Above, Iterations, CDF_tmp, Curr, LoCPerc, AggDedPerc) 
{
    ptm <- proc.time()
    xrtoUSD <- Xrates[Xrates[1] == Curr, 2]
    RetentionUSD <- Retention * xrtoUSD
    DeductibleUSD <- Deductible * xrtoUSD
    UpperLimit = ifelse(Within == "Yes", RetentionUSD, RetentionUSD + 
        DeductibleUSD)
    if (Above == "Ground Up") 
        GUClaims = Claims
    else GUClaims = Claims/(1 - approx(CDF_tmp[, 1], CDF_tmp[, 
        1 + IndustryIndex], DeductibleUSD)$y)
    N <- array(0, dim = c(Iterations))
    if (GUClaims < 75) 
        N <- rpois(Iterations, GUClaims)
    else N <- rnorm(Iterations, GUClaims, sqrt(GUClaims))
    MaxN <- max(N)
    Losses <- array(0, dim = c(MaxN, Iterations))
    for (i in seq(from = 1, to = Iterations, by = 1)) {
        if (N[i] > 0) 
            Losses[1:N[i], i] <- approx(CDF_tmp[, 1 + IndustryIndex], 
                CDF_tmp[, 1], runif(N[i]))$y
    }
    LossesInLayer <- pmax((pmin(Losses, UpperLimit) - Deductible), 
        0)
    AggregateLossInLayer <- colSums(LossesInLayer, dim = 1)
    MeanLoss <- mean(AggregateLossInLayer)
    LoC <- quantile(x = AggregateLossInLayer, probs = LoCPerc/100, 
        na.rm = TRUE)/MeanLoss
    AggDed <- quantile(x = AggregateLossInLayer, probs = AggDedPerc/100, 
        na.rm = TRUE)/MeanLoss
    Elapsedtime <- proc.time() - ptm
    MeanLossOrig <- MeanLoss/xrtoUSD
    list(dist = AggregateLossInLayer/xrtoUSD, stats = c(`Mean LC` = MeanLossOrig, 
        `Ratio LoC to Mean` = LoC[[1]], LoC = MeanLossOrig * 
            LoC[[1]], `Ratio Agg Ded to Mean` = AggDed[[1]], 
        `Agg Ded` = MeanLossOrig * AggDed[[1]], `GU Claims` = GUClaims, 
        `Simulation time` = Elapsedtime[3]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
