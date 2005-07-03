\name{melt}
\alias{melt}
\title{Melt}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Melt a data frame into form suitable for easy casting.
}
\usage{melt(data, id.var, measure.var, variable_name = "variable", preserve.na = FALSE)}
\arguments{
\item{data}{Data set to melt}
\item{id.var}{Identifying variables. If blank, will use all non measure.var variables}
\item{measure.var}{Measured variables. If blank, will use all non id.var variables}
\item{variable_name}{Should NAs be preserved or dropped from the data set?}
\item{preserve.na}{}
}
\value{molten data}
\details{Along with \link{cast} and \link{recast}, melt is the only function from this package
you will actually use.  The rest are all support functions for these two.
melt gets your data into a suitable form (molten) for cast to work on.

You need to tell melt which of your variables are id variables, and
which are measured variables.  For most practical uses, the id variables
will be categorical, and the measured variables continuous.   If you only
supply one of id.var and measure.var, melt will assume the remainder of
the variables in the data set belong to the other.  If you supply neither,
melt will assume integer and factor  variables are id variables,
and all other are measured.}
\seealso{\url{http://had.co.nz/reshape/}}
\examples{data(tips)
head(melt(tips))
names(airquality) <- tolower(names(airquality))
airquality.d <- melt(airquality, id=c("month", "day"))
head(airquality.d)
names(ChickWeight) <- tolower(names(ChickWeight))
chick.d <- melt(ChickWeight, id=2:4)}
\keyword{manip}
