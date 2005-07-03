# Cast function
# Cast a molten data frame into the reshaped or aggregated form you want
#
# Along with \code{\link{melt}}  and \link{recast}, this is the only function you should ever need to use.
# Once you have melted your data, cast will arrange it into the form you desire
# based on the specification given by \code{formula}.
#
# The cast formula has the following format: \code{x_variable + x_2 ~ y_variable + y_2 ~ z_variable ~  ... | list_variable + ... }
# The order of the variables makes a difference.  The first varies slowest, and the last 
# fastest.  There are a couple of special variables: "..." represents all other variables 
# not used in the formula and "." represents no variable, so you can do \code{formula=var1 ~ .}
#
# Creating high-D arrays is simple, and allows a class of transformations that are hard
# without \code{\link{iapply}} and \code{\link{sweep}} 
#
# If the combination of variables you supply does not uniquely identify one row in the 
# original data set, you will need to supply an aggregating function, \code{fun.aggregate}.
# This function should take a vector of numbers and return a summary statistic(s).  It must
# return the same number of arguments regardless of the length of the input vector.
# If it returns multiple value you can use "result\_variable" to control where they appear.
# By default they will appear as the last column variable.
#
# The margins argument should be passed a vector of variable names, eg.
# \code{c("month","day")}.  It will silently drop any variables that can not be margined 
# over.  You can also use "grand\_col" and "grand\_row" to get grand row and column margins
# respectively.
#
# Subset takes a logical vector that will be evaluated in the context of \code{data},
# so you can do something like \code{subset = variable=="length"}
#
# All the actual reshaping is done by \code{\link{reshape1}}, see its documentation
# for details of the implementation
#
# @keyword manip
# @arguments molten data frame, see \code{\link{melt}}
# @arguments casting formula, see details for specifics
# @arguments aggregation function
# @arguments further arguments are passed to aggregating function
# @arguments vector of variable names (can include "grand\_col" and "grand\_row") to compute margins for, or TRUE to computer all margins
# @arguments logical vector to subset data set with before reshaping
# @seealso \code{\link{reshape1}}, vignette("introduction", "reshape"), \url{http://had.co.nz/reshape/}
#X #Air quality example
#X names(airquality) <- tolower(names(airquality))
#X airquality.d <- melt(airquality, id=c("month", "day"), preserve.na=FALSE)
#X 
#X cast(airquality.d, day ~ month ~ variable)
#X cast(airquality.d, month ~ variable, mean)
#X cast(airquality.d, month ~ . | variable, mean)
#X cast(airquality.d, month ~ variable, mean, margins=c("grand_row", "grand_col"))
#X cast(airquality.d, day ~ month, mean, subset=variable=="ozone")
#X cast(airquality.d, month ~ variable, range)
#X cast(airquality.d, month ~ variable + result_variable, range)
#X cast(airquality.d, variable ~ month ~ result_variable,range)
#X
#X #Chick weight example
#X names(ChickWeight) <- tolower(names(ChickWeight))
#X chick.d <- melt(ChickWeight, id=2:4, preserve.na = FALSE)
#X 
#X cast(chick.d, time ~ variable, mean) # average effect of time
#X cast(chick.d, diet ~ variable, mean) # average effect of diet
#X cast(chick.d, diet ~ time ~ variable, mean) # average effect of diet & time
#X 
#X # How many chicks at each time? - checking for balance
#X cast(chick.d, time ~ diet, length)
#X cast(chick.d, chick ~ time, mean)
#X cast(chick.d, chick ~ time, mean, subset=time < 10 & chick < 20)
#X 
#X cast(chick.d, diet + chick ~ time)
#X cast(chick.d, chick ~ time ~ diet)
#X cast(chick.d, diet + chick ~ time, mean, margins="diet")
#X
#X #Tips example
#X cast(melt(tips), sex ~ smoker, mean, subset=variable=="total_bill")
#X cast(melt(tips), sex ~ smoker | variable, mean)
#X 
#X ff_d <- melt(french_fries, id=1:4, preserve.na=FALSE)
#X cast(ff_d, subject ~ time, length)
#X cast(ff_d, subject ~ time, function(x) 30 - length(x))
#X cast(ff_d, variable ~ ., function(x) c(min=min(x), max=max(x)))
#X cast(ff_d, variable ~ ., function(x) quantile(x,c(0.25,0.5)))
#X cast(ff_d, treatment ~ variable, mean, margins=c("grand_col", "grand_row"))
#X cast(ff_d, treatment + subject ~ variable, mean, margins="treatment")
#X lattice::xyplot(X1 ~ X2 | variable, cast(ff_d, ... ~ rep), aspect="iso")
cast <- function(data, formula = ... ~ variable, fun.aggregate=NULL, ..., margins=FALSE, subset=TRUE, df=FALSE) {
  if (!is.character(formula)) formula <- deparse(substitute(formula))
	subset <- eval(substitute(subset), data, parent.frame())  
	data <- data[subset, ]  
	variables <- cast_parse_formula(formula, names(data))
	
	if (!is.null(variables$l)) {
		res <- nested.by(data, data[variables$l], function(x) {
			reshape1(x, variables$m, fun.aggregate, margins=margins, df=df, ...)
		})	
	} else {
		res <- reshape1(data, variables$m, fun.aggregate, margins=margins, df=df, ...)
	}
	attr(res, "formula") <- formula
	#attr(res, "data") <- deparse(substitute(data))
	
	res
}

# Casting workhorse.
# Takes data frame and variable list and casts data.
#
# @arguments data frame
# @arguments variables to appear in columns
# @arguments variables to appear in rows
# @arguments aggregation function
# @arguments should the aggregating function be supplied with the entire data frame, or just the relevant entries from the values column
# @arguments vector of variable names (can include "grand\_col" and "grand\_row") to compute margins for, or TRUE to computer all margins
# @arguments further arguments are passed to aggregating function
# @seealso \code{\link{cast}}
# @keyword internal
#X names(airquality) <- tolower(names(airquality))
#X airquality.d <- melt(airquality, id=c("month", "day"), preserve=FALSE)
#X #Basic call
#X reshape1(airquality.d, list("month", NULL), mean)
#X reshape1(airquality.d, list("month", "variable"), mean)
#X reshape1(airquality.d, list("day", "month"), mean)
#X 
#X #Explore margins
#X reshape1(airquality.d, list("month", NULL), mean, "month")
#X reshape1(airquality.d, list("month", NULL) , mean, "grand_col")
#X reshape1(airquality.d, list("month", NULL) , mean, "grand_row")
#X 
#X reshape1(airquality.d, list(c("month", "day"), NULL), mean, "month")
#X reshape1(airquality.d, list(c("month"), "variable"), mean, "month")
#X reshape1(airquality.d, list(c("variable"), "month"), mean, "month")
#X reshape1(airquality.d, list(c("month"), "variable"), mean, c("month","variable"))
#X 
#X reshape1(airquality.d, list(c("month"), "variable"), mean, c("grand_row"))
#X reshape1(airquality.d, list(c("month"), "variable"), mean, c("grand_col"))
#X reshape1(airquality.d, list(c("month"), "variable"), mean, c("grand_row","grand_col"))
#X 
#X reshape1(airquality.d, list(c("variable","day"),"month"), mean,c("variable"))
#X reshape1(airquality.d, list(c("variable","day"),"month"), mean,c("variable","grand_row"))
#X reshape1(airquality.d, list(c("month","day"), "variable"), mean, "month") 
#X 
#X # Multiple fnction returns
#X reshape1(airquality.d, list(c("month", "result_variable"), NULL), range) 
#X reshape1(airquality.d, list(c("month"),"result_variable") , range) 
#X reshape1(airquality.d, list(c("result_variable", "month"), NULL), range) 
#X 
#X reshape1(airquality.d, list(c("month", "result_variable"), "variable"), range, "month")
#X reshape1(airquality.d, list(c("month", "result_variable"), "variable"), range, "variable")
#X reshape1(airquality.d, list(c("month", "result_variable"), "variable"), range, c("variable","month"))
#X reshape1(airquality.d, list(c("month", "result_variable"), "variable"), range, c("grand_col"))
#X reshape1(airquality.d, list(c("month", "result_variable"), "variable"), range, c("grand_row"))
#X 
#X reshape1(airquality.d, list(c("month"), c("variable")), function(x) diff(range(x))) 
reshape1 <- function(data, vars = list(NULL, NULL), fun.aggregate=NULL, margins, df=FALSE, ...) {
	vars.clean <- lapply(vars, clean.vars)
	variables <- unlist(vars.clean)

	if (!missing(margins) && isTRUE(margins)) margins <- c(variables, "grand_row", "grand_col")
	
	aggregate <- nrow(unique(data[,variables, drop=FALSE])) < nrow(data)
	if (aggregate) {
		if (missing(fun.aggregate) || is.null(fun.aggregate)) {
			warning("Aggregation requires fun.aggregate: length used as default", call.=FALSE)
			fun.aggregate <- length
		}
		if (!df) {
		  data.r <- expand(condense(data, variables, fun.aggregate, ...)) 
		} else {
		  data.r <- condense.df(data, variables, fun.aggregate, ...)
		}

		if ("result_variable" %in% names(data.r) && !("result_variable" %in% unlist(vars))) {
			vars[[2]] <- c(vars[[2]], "result_variable")
		}
	} else {
		data.r <- data.frame(data[,c(variables)], result = data$value)	
	}
  
  if (length(vars.clean) > 2 && margins) {
    warning("Sorry, you currently can't use margins with high D arrays", .call=FALSE)
    margins <- FALSE
  }
	margins.r <- compute.margins(data, margin.vars(vars.clean, margins), fun.aggregate, ..., df=df)
	result <- sort.df(rbind.fill(data.r, margins.r), unlist(vars))
	result <- add.all.combinations(result, vars)

	dimnames <- lapply(vars, function(x) dim.names(result, x))

  r <- if (!df) unlist(result$result) else result$result
	reshaped <- array(r, rev(sapply(dimnames, nrow)))
  
  reshaped <- aperm(reshaped, length(dim(reshaped)):1)
	dimnames(reshaped) <- lapply(dimnames, function(x) apply(x, 1, paste, collapse="-"))
	rownames(reshaped) <- 1:nrow(reshaped)
	
	if (length(vars.clean) > 2) return(reshaped)
	if (df) return(cast_matrix(reshaped, dimnames))
	as.data.frame(cast_matrix(reshaped, dimnames))
}


# Add all combinations
# Add all combinations of the given rows and columns to the data frames.
# 
# This function is used to ensure that we have a matrix of the appropriate
# dimensionaliy with no missing cells.
# 
# @arguments data.frame
# @arguments variables (list of character vectors)
# @keyword internal 
#X rdunif <- 
#X   function(n=20, min=0, max=10) floor(runif(n,min, max))
#X df <- data.frame(a = rdunif(), b = rdunif(),c = rdunif())
#X add.all.combinations(df)
#X add.all.combinations(df, list("a", "b"))
#X add.all.combinations(df, list(c("a", "b")))
#X add.all.combinations(df, list("a", "b", "c"))
#X add.all.combinations(df, list(c("a", "b"), "c"))
#X add.all.combinations(df, list(c("a", "b", "c")))
add.all.combinations <- function(data, vars = list(NULL)) {
	if (sum(sapply(vars, length)) == 0) return(data)

	all.combinations <- do.call(expand.grid.df, 
		lapply(vars, function(cols) data[, cols, drop=FALSE])
	)
	result <- merge_recurse(list(data, all.combinations)) 
	
	sort.df(result, unlist(vars))
}


# Compute margins
# Compute marginal values.
# 
# @arguments data frame
# @arguments margins to compute
# @arguments aggregation function
# @arguments other argument passed to aggregation function
# @keyword internal 
compute.margins <- function(data, margins, fun.aggregate, ..., df=FALSE) {
	if (length(margins) == 0) return(data.frame())
	
	if (missing(fun.aggregate)) {
		warning("Margins require fun.aggregate: length used as default", call.=FALSE)
		fun.aggregate <- length
	}
	exp <- function(x) {
		if (df) {
			condense.df(data, x, fun.aggregate, ...)
		} else {
			expand(condense(data, x, fun.aggregate, ...))
		}
	}
	
	do.call("rbind.fill",lapply(margins, exp))
}


# Margin variables
# Works out list of variables to margin over to get desired margins.
#
# Variables that can't be margined over are dropped silently.
#
# @arguments column variables
# @arguments row variables
# @arguments vector of variable names to margin over.
# @keyword internal
margin.vars <- function(vars = list(NULL, NULL), margins = NULL) {
	rows <- vars[[1]]
	cols <- vars[[2]]
	if (missing(margins) || is.null(margins)) return(NULL)
	
	# Nothing to margin over for last variable in column or row
	row.margins <- intersect(rows[-length(rows)], margins)
	col.margins <- intersect(cols[-length(cols)], margins)

	grand.row <- "grand_row" %in% margins
	grand.col <- "grand_col" %in% margins
	
	margin.intersect <- function(cols, col.margins, rows, row.margins) {
		unlist(lapply(col.margins, function(col) {
			c(lapply(row.margins, c, col), list(c(col, rows)))
		}), recursive = FALSE)
	}
	
	margins.all <- c(
		margin.intersect(cols, col.margins, rows, row.margins),  
		margin.intersect(rows, row.margins, cols, col.margins)
	)

	if (grand.row && !is.null(rows)) margins.all <- c(margins.all, list(cols), list(col.margins))
	if (grand.col && !is.null(cols)) margins.all <- c(margins.all, list(rows), list(row.margins))
	if (grand.col && grand.row  && !is.null(rows)  && !is.null(cols)) margins.all <- c(margins.all, list(numeric(0)))
	
	duplicates <- duplicated(lapply(lapply(margins.all,function(x) if(!is.null(x)) sort(x)), paste, collapse=""))
	
	margins.all[!duplicates]
}

# Dimension names
# Convenience method for extracting row and column names 
# 
# @arguments data frame
# @arguments variables to use
# @keyword internal
dim.names <- function(data, vars) {
	if (!is.null(vars) && length(vars) > 0) {
		unique(data[,vars,drop=FALSE]) 
	} else {
		data.frame(value="value") # use fun.aggregate instead of "value"? 
	}
}
