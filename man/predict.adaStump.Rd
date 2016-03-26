\name{predict.adaStump}
\alias{predict.adaStump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Predict from an adaStump object
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.adaStump(stump.model, newdata, n.iter = nrow(stump.model$model))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stump.model}{
%%     ~~Describe \code{stump.model} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
}
  \item{n.iter}{
%%     ~~Describe \code{n.iter} here~~
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
function (stump.model, newdata, n.iter = nrow(stump.model$model)) 
{
    if (class(stump.model) != "adaStump") 
        stop("Not an adaStump Model")
    if (nrow(stump.model$model) == 0) 
        return(rep(NA, nrow(newdata)))
    cols <- unique(stump.model$model$var)
    newdata_reduced <- newdata[, cols, drop = F]
    rm(newdata)
    stumps.probs <- sapply(1:n.iter, function(x) {
        args.st <- as.list(stump.model$model[x, ])
        args.st$newdata <- newdata_reduced
        args.st$value <- unlist(strsplit(args.st$value, split = ","))
        %if (all(grepl("^[0-9\\.]+$", args.st$value))) 
            args.st$value <- as.numeric(args.st$value)
        args.st$type <- stump.model$type
        ret <- do.call("getCoefficientLine", args.st)
        return(ret)
    })
    coefs <- apply(stumps.probs, 1, sum)
    final.prob <- sapply(coefs, function(x) {
        mod.coef <- ifelse(x > 0, exp(-2 * x), exp(2 * x))
        ret <- mod.coef/(1 + mod.coef)
        return(ret)
    })
    return(final.prob)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
