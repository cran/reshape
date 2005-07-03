\name{condense}
\alias{condense}
\title{Condense}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Condense a data frame.
}
\usage{condense(data, variables, fun, ...)}
\arguments{
\item{data}{data frame}
\item{variables}{variables to condense over}
\item{fun}{aggregating function, maybe return mutliple values}
\item{...}{further arguments passed on to aggregating function}
}

\details{Works very much like by, but keeps data in original data frame format.
Results column is a list, so that each cell may contain an object or a vector etc.
Assumes data is in deshaped format. Aggregating function must return the
same number of arguments for all input.}

\examples{}
\keyword{manip}
