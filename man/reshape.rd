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
\examples{names(airquality) <- tolower(names(airquality))
airquality.d <- melt(airquality, id=c("month", "day"))
#Basic call
reshape1(airquality.d, c("month"), , mean)
reshape1(airquality.d, c("month"), "variable", mean)
reshape1(airquality.d, c("day"), "month", mean)

#Explore margins
reshape1(airquality.d, c("month"), , mean, "month")
reshape1(airquality.d, c("month"), , mean, "grand_col")
reshape1(airquality.d, c("month"), , mean, "grand_row")

reshape1(airquality.d, c("month", "day"), , mean, "month")
reshape1(airquality.d, c("month"), "variable", mean, "month")
reshape1(airquality.d, c("variable"), "month", mean, "month")
reshape1(airquality.d, c("month"), "variable", mean, c("month","variable"))

reshape1(airquality.d, c("month"), "variable", mean, c("grand_row"))
reshape1(airquality.d, c("month"), "variable", mean, c("grand_col"))
reshape1(airquality.d, c("month"), "variable", mean, c("grand_row","grand_col"))

reshape1(airquality.d, c("variable","day"),"month", mean,c("variable"))
reshape1(airquality.d, c("variable","day"),"month", mean,c("variable","grand_row"))
reshape1(airquality.d, c("month","day"), "variable", mean, "month") 

# Multiple fnction returns
reshape1(airquality.d, c("month", "result_variable"), , range) 
reshape1(airquality.d, c("month"),"result_variable" , range) 
reshape1(airquality.d, c("result_variable", "month"), , range) 

reshape1(airquality.d, c("month", "result_variable"), "variable", range, "month")
reshape1(airquality.d, c("month", "result_variable"), "variable", range, "variable")
reshape1(airquality.d, c("month", "result_variable"), "variable", range, c("variable","month"))
reshape1(airquality.d, c("month", "result_variable"), "variable", range, c("grand_col"))
reshape1(airquality.d, c("month", "result_variable"), "variable", range, c("grand_row"))

reshape1(airquality.d, c("month"), c("variable"), function(x) diff(range(x))) }
\keyword{manip}
