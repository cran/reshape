\name{deshape}
\alias{deshape}
\title{Deshape}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Deshape a data frame into form suitable for easy reshaping.
}
\usage{deshape(data, id.var, measure.var, variable_name = "variable")}
\arguments{
\item{data}{data Data set to deshape}
\item{id.var}{id.var Identifying variables. If blank, will use all non measure.var variables}
\item{measure.var}{measure.var Measured variables. If blank, will use all non id.var variables}
\item{variable_name}{}
}
\value{Deshaped data.frame}
\details{Along with reshape, deshape is the only function from this package
you will actually use.  The rest are all support functions for these two.
Deshape gets your data into a form suitable for reshape to work on.

You need to tell deshape which of your variables are id variables, and
which are measured variables.  For most practical uses, the id variables
will be categorical, and the measured variables continuous.

Note that if you only supply one of id.var and measure.var, deshape
will assume the remainder of the variables in the data set belong to
the other.  If you supply neither, deshape will assume integer and factor
variables are id variables, and all other are measured.}

\examples{data(tips)
head(deshape(tips))
names(airquality) <- tolower(names(airquality))
airquality.d <- deshape(airquality, id=c("month", "day"))
head(airquality.d)
names(ChickWeight) <- tolower(names(ChickWeight))
chick.d <- deshape(ChickWeight, id=2:4)}
\keyword{manip}
