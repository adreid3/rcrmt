\name{resultsPlot}
\alias{resultsPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
resultsPlot(datasetInput)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datasetInput}{
%%     ~~Describe \code{datasetInput} here~~
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
function (datasetInput) 
{
    results <- try(as.character(datasetInput[[2]]))
    if (class(results) != "try-error") {
        dist <- datasetInput$dist
        stats <- datasetInput$stats
        inputs <- datasetInput$inputs
        myhist <- hist(dist, xlim = c(0, max(dist)), cex.lab = 1.2, 
            cex.main = 1.5, col = gray(0.8), breaks = 50, main = paste("Aggregate Loss Distribution for ", 
                inputs$AccountName, sep = ""), xlab = paste("Aggregate Loss (", 
                Curr, ")", sep = ""))
        mydensity <- density(dist)
        multiplier <- myhist$counts/myhist$density
        mydensity$y <- mydensity$y * multiplier[1]
        lines(mydensity, lwd = 2)
        if (dist[1] > 0) {
            if (inputs$Within == "Yes") 
                limit <- inputs$Retention
            else limit <- inputs$Retention + inputs$Deductible
            abline(v = limit, col = "black", lwd = 4)
            abline(v = inputs$Deductible, col = "black", lwd = 4)
            abline(v = stats[1], col = "red", lwd = 4)
            abline(v = stats[3], col = "blue", lwd = 4)
            abline(v = stats[5], col = "green", lwd = 4)
            text(x = inputs$Deductible, y = max(myhist$counts), 
                labels = paste("Deductible (", format(inputs$Deductible, 
                  big.mark = ",", scientific = F), ")", sep = ""), 
                adj = c(-0.1, 0))
            text(x = limit, y = max(myhist$counts), labels = paste("Top of Layer (", 
                format(limit, big.mark = ",", scientific = F), 
                ")", sep = ""), adj = c(-0.1, 2))
            text(x = stats[1], y = max(myhist$counts), labels = paste("Mean LC (", 
                format(round(stats[1], 0), big.mark = ",", scientific = F), 
                ")", sep = ""), adj = c(-0.1, 4))
            text(x = stats[3], y = max(myhist$counts), labels = paste("85th LC (", 
                format(round(stats[3], 0), big.mark = ",", scientific = F), 
                ")", sep = ""), adj = c(-0.1, 8))
            text(x = stats[5], y = max(myhist$counts), labels = paste("Agg Ded (", 
                format(round(stats[5], 0), big.mark = ",", scientific = F), 
                ")", sep = ""), adj = c(-0.1, 10))
        }
    }
    else {
        histdata <- c(0, 0)
        linedata <- c(0, 0)
        hist(histdata, xlim = c(0, 1e+07), main = paste("Aggregate Loss Distribution for", 
            inputs$AccountName), xlab = paste("Aggregate Loss (", 
            inputs$Curr, ")"))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
