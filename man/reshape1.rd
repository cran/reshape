\name{reshape1}
\alias{reshape1}
\title{Casting workhorse.}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Takes data frame and variable list and casts data.
}
\usage{reshape1(data, rows = NULL, cols = NULL, fun.aggregate, margins, ...)}
\arguments{
\item{data}{data frame}
\item{rows}{variables to appear in columns}
\item{cols}{variables to appear in rows}
\item{fun.aggregate}{aggregation function}
\item{margins}{vector of variable names (can include "grand\_col" and "grand\_row") to compute margins for, or TRUE to computer all margins}
\item{...}{further arguments are passed to aggregating function}
}

\details{}
\seealso{\code{\link{cast}}}
\examples{}
\keyword{manip}
