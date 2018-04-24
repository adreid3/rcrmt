\name{resultsTable}
\alias{resultsTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Title
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
resultsTable(datasetInput)
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
    if (class(results) == "try-error") 
        results <- c("", "", "", "", "", "", "")
    else {
        rounddecimals <- c(0, 2, 0, 2, 0, 0, 3)
        roundedvalues <- round(datasetInput[[2]], rounddecimals)
        for (i in 1:length(rounddecimals)) {
            results[i] <- as.character(format(roundedvalues[i], 
                big.mark = ",", scientific = F))
        }
    }
    data.frame(Quantity = c("Mean Loss Cost", "Ratio of Letter of Credit (LoC) Required to the Mean", 
        "Required LoC", "Ratio of Aggregate Deductible to the Mean", 
        "Aggregate Deductible", "Expected Number of Claims (Ground up)", 
        "Simulation run time (s)"), Value = results, stringsAsFactors = FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
