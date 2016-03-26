\name{adaStump}
\alias{adaStump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Fit an ada boosting model with decision stumps as base learners
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adaStump(formula, data, type, iter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
  formula object describing the model to be fit, as in ada function from "ada" package.
  
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
  data.frame object  to train the model. The variables named in formula must be present in this object
  
%%     ~~Describe \code{data} here~~
}
  \item{type}{
  
  Type of boosting algorithm, as in ada function from "ada" package
%%     ~~Describe \code{type} here~~
}
  \item{iter}{
%%     ~~Describe \code{iter} here~~
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
function (formula, data, type, iter) 
{
    args <- list(formula = formula, data = data, type = type, 
        iter = iter, control = rpart.control(maxdepth = 1, cp = -1, 
            minsplit = 0, xval = 0))
    model <- do.call("ada", args)
    ret <- createStumpFrame(model)
    ret <- list(model = ret, type = type)
    class(ret) <- "adaStump"
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
