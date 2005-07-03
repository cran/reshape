\name{reshape}
\alias{reshape}
\title{Master reshape function}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Reshaped or aggregate a deshaped data set
}
\usage{reshape(data, formula = ... ~ variable, fun.aggregate, ..., margins, subset=TRUE)}
\arguments{
\item{data}{deshaped data frame, see \code{\link{deshape}}}
\item{formula}{reshaping formula, see details for specifics}
\item{fun.aggregate}{aggregation function}
\item{...}{further arguments are passed to aggregating function}
\item{margins}{vector of variable names (can include "grand\_col" and "grand\_row") to create margins over}
\item{subset}{logical vector to subset data set before reshaping}
}

\details{Along with \code{\link{deshape}}, this is the only function you should ever need to use.
Once you have deshaped your data, reshape will arrange it into the form you desire
based on the specification given by \code{formula}.

The reshaping formula has the following format: \code{row_variable\_1 + row\_2 ~ col\_variable + col\_2}
The order of the variables makes a difference.  The first varies slowest, and the last
fastest.  There are a couple of special variables: "..." represents all other variables
not used in the formula and "." represents no variable, so you can do \code{formula=var1 ~ .}

If the combination of variables you supply does not uniquely identify one row in the
original data set, you will need to supply an aggregating function, \code{fun.aggregate}.
This function should take a vector of numbers and return a summary statistic(s).  It must
return the same number of arguments regardless of the length of the input vector.
If it returns multiple value you can use "result\_variable" to control where they appear.
By default they will appear as the last column variable.

The margins argument should be passed a vector of variable names, eg.
\code{c("month","day")}.  It will silently drop any variables that can not be marginned
over.  You can also use "grand\_col" and "grand\_row" to give grand row and column margins
respectively.

Subset takes a logical vector that will be evaluated in the context of \code{data},
so you can do something like \code{subset= variable=="length"}

All the actual reshaping is done by \code{\link{reshape1}}, see its documentation
for details of the implementation}
\seealso{\code{\link{reshape1}}}
\examples{#Air quality example
names(airquality) <- tolower(names(airquality))
airquality.d <- deshape(airquality, id=c("month", "day"))

reshape(airquality.d, month ~ variable, mean)
reshape(airquality.d, month ~ variable, mean, margins=c("grand_row", "grand_col"))
reshape(airquality.d, day ~ month, mean, subset=variable=="ozone")

#Chick weight example
names(ChickWeight) <- tolower(names(ChickWeight))
chick.d <- deshape(ChickWeight, id=2:4)

reshape(chick.d, time ~ variable, mean) # average effect of time
reshape(chick.d, diet ~ variable, mean) # average effect of diet

# How many chicks at each time? - checking for balance
reshape(chick.d, time ~ diet, length)
reshape(chick.d, chick ~ time, mean)
reshape(chick.d, chick ~ time, mean, subset=time < 10 & chick < 20)

reshape(chick.d, diet + chick ~ time)
reshape(chick.d, diet + chick ~ time, mean, margins="diet")
#Tips example
data(tips)
reshape(deshape(tips), sex ~ smoker, mean, subset=variable=="total_bill")}
\keyword{manip}
