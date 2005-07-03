\name{cast}
\alias{cast}
\title{Cast function}
\author{Hadley Wickham <h.wickham@gmail.com>}

\description{
Cast a molten data frame into the reshaped or aggregated form you want
}
\usage{cast(data, formula = ... ~ variable, fun.aggregate, ..., margins, subset=TRUE)}
\arguments{
\item{data}{molten data frame, see \code{\link{melt}}}
\item{formula}{casting formula, see details for specifics}
\item{fun.aggregate}{aggregation function}
\item{...}{further arguments are passed to aggregating function}
\item{margins}{vector of variable names (can include "grand\_col" and "grand\_row") to compute margins for, or TRUE to computer all margins}
\item{subset}{logical vector to subset data set with before reshaping}
}

\details{Along with \code{\link{melt}}  and \link{recast}, this is the only function you should ever need to use.
Once you have melted your data, cast will arrange it into the form you desire
based on the specification given by \code{formula}.

The cast formula has the following format: \code{row_variable_1 + row_2 ~ col_variable + col_2}
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
\code{c("month","day")}.  It will silently drop any variables that can not be margined
over.  You can also use "grand\_col" and "grand\_row" to get grand row and column margins
respectively.

Subset takes a logical vector that will be evaluated in the context of \code{data},
so you can do something like \code{subset = variable=="length"}

All the actual reshaping is done by \code{\link{reshape1}}, see its documentation
for details of the implementation}
\seealso{\code{\link{reshape1}}, \url{http://had.co.nz/reshape/}}
\examples{#Air quality example
names(airquality) <- tolower(names(airquality))
airquality.d <- melt(airquality, id=c("month", "day"))

cast(airquality.d, month ~ variable, mean)
cast(airquality.d, month ~ variable, mean, margins=c("grand_row", "grand_col"))
cast(airquality.d, day ~ month, mean, subset=variable=="ozone")
cast(airquality.d, month ~ variable, range)
cast(airquality.d, month ~ variable + result_variable, range)

#Chick weight example
names(ChickWeight) <- tolower(names(ChickWeight))
chick.d <- melt(ChickWeight, id=2:4)

cast(chick.d, time ~ variable, mean) # average effect of time
cast(chick.d, diet ~ variable, mean) # average effect of diet

# How many chicks at each time? - checking for balance
cast(chick.d, time ~ diet, length)
cast(chick.d, chick ~ time, mean)
cast(chick.d, chick ~ time, mean, subset=time < 10 & chick < 20)

cast(chick.d, diet + chick ~ time)
cast(chick.d, diet + chick ~ time, mean, margins="diet")
#Tips example
data(tips)
cast(melt(tips), sex ~ smoker, mean, subset=variable=="total_bill")

data(french_fries)
ff_d <- melt(french_fries, id=1:4)
cast(ff_d, subject ~ time, length)
cast(ff_d, subject ~ time, function(x) 30 - length(x))
cast(ff_d, variable ~ ., function(x) c(min=min(x), max=max(x)))
cast(ff_d, treatment ~ variable, mean, margins=c("grand_col", "grand_row"))
cast(ff_d, treatment + subject ~ variable, mean, margins="treatment", subset=subject < 5)
lattice::xyplot(X1 ~ X2 | variable, cast(ff_d, ... ~ rep), aspect="iso")}
\keyword{manip}
