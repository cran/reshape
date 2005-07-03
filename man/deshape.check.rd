\name{deshape.check}
\alias{deshape.check}
\title{Deshape check.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Check that input variables to deshape are appropriate.
}
\usage{deshape.check(data, id.var, measure.var)}
\arguments{
\item{data}{variable names}
\item{id.var}{Vector of identifying variable names or indexes}
\item{measure.var}{Vector of Measured variable names or indexes}
}
\value{
 \item{id list id variable names}
 \item{measure list of measured variable names}
}
\details{If id.var or measure.var are missing, deshape.check will do it's
best to impute them.}

\examples{}
\keyword{manip}
