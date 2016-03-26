\name{getCoefficientLine}
\alias{getCoefficientLine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Internal - Get individual tree output from stump.frame object
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getCoefficientLine(var, condition, value, probT, probF, alpha, newdata, default, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{condition}{
%%     ~~Describe \code{condition} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
}
  \item{probT}{
%%     ~~Describe \code{probT} here~~
}
  \item{probF}{
%%     ~~Describe \code{probF} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
}
  \item{default}{
%%     ~~Describe \code{default} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (var, condition, value, probT, probF, alpha, newdata, 
    default, type) 
{
    meets.condition <- do.call(condition, list(newdata[[var]], 
        value))
    meets.condition[is.na(newdata[[var]])] <- default
    tree.value <- ifelse(meets.condition, probT, probF)
    switch(type, real = {
        ind <- tree.value == 1 | (1 - tree.value) == 1
        if (length(ind) > 1) {
            tree.value[ind] = (1 - tree.value[ind]) * 1e-04 + 
                tree.value[ind] * 0.9999
        }
        modified.value <- 0.5 * log(tree.value/(1 - tree.value))
        ret <- modified.value * alpha
    }, discrete = {
        ret <- tree.value * alpha
    }, gentle = {
        ret <- tree.value * alpha
    })
    return(ret)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
