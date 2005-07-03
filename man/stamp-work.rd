\name{stamp.work}
\alias{stamp.work}
\title{Stamp work}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
This is the workhouse that powers the stamp function.
}
\usage{stamp.work(data, rows = NULL, cols = NULL, fun, margins=NULL, ...)}
\arguments{
\item{data}{data frame}
\item{rows}{row variables (character vector)}
\item{cols}{column variables (character vector)}
\item{fun}{stamping function, should take a dataframe as it's first arugment}
\item{margins}{variables to margin over (character vector, or TRUE, for all margins)}
\item{...}{arguments passed to stamping function}
}

\details{You shouldn't call this function, please use \code{\link{stamp}}
instead.}

\examples{}
\keyword{manip}
