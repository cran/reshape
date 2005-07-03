\name{iapply}
\alias{iapply}
\title{Idempotent apply}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
A version of apply that works like apply, but returns the array
}
\usage{iapply(x, margins=1, fun, ..., REDUCE=TRUE)}
\arguments{
\item{x}{array}
\item{margins}{margins to apply over}
\item{fun}{function to apply}
\item{...}{other arguments pass to function}
\item{REDUCE}{reduce extra dimension?}
}

\details{Applied function should return an array, matrix or vector.}

\examples{a <- array(1:27, c(2,3,4))
all.equal(a, iapply(a, 1, force))
all.equal(a, iapply(a, 1:2, force))
all.equal(a, iapply(a, 1:3, force))
all.equal(aperm(a, c(2,1,3)), iapply(a, 2, force))
all.equal(aperm(a, c(3,1,2)), iapply(a, 3, force))

iapply(a, 1, min)   
iapply(a, 2, min)   
iapply(a, 3, min)   
iapply(a, 1, range)   
iapply(a, 2, range)   
iapply(a, 3, range)   }
\keyword{manip}
