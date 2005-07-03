# Master reshape function
# Reshaped or aggregate a deshaped data set
#
# Along with \code{\link{deshape}}, this is the only function you should ever need to use.
# Once you have deshaped your data, reshape will arrange it into the form you desire
# based on the specification given by \code{formula}.
#
# The reshaping formula has the following format: \code{row_variable\_1 + row\_2 ~ col\_variable + col\_2}
# The order of the variables makes a difference.  The first varies slowest, and the last 
# fastest.  There are a couple of special variables: "..." represents all other variables 
# not used in the formula and "." represents no variable, so you can do \code{formula=var1 ~ .}
#
# If the combination of variables you supply does not uniquely identify one row in the 
# original data set, you will need to supply an aggregating function, \code{fun.aggregate}.
# This function should take a vector of numbers and return a summary statistic(s).  It must
# return the same number of arguments regardless of the length of the input vector.
# If it returns multiple value you can use "result\_variable" to control where they appear.
# By default they will appear as the last column variable.
#
# The margins argument should be passed a vector of variable names, eg.
# \code{c("month","day")}.  It will silently drop any variables that can not be marginned 
# over.  You can also use "grand\_col" and "grand\_row" to give grand row and column margins
# respectively.
#
# Subset takes a logical vector that will be evaluated in the context of \code{data},
# so you can do something like \code{subset= variable=="length"}
#
# All the actual reshaping is done by \code{\link{reshape1}}, see its documentation
# for details of the implementation
#
# @keyword manip
# @arguments deshaped data frame, see \code{\link{deshape}}
# @arguments reshaping formula, see details for specifics
# @arguments aggregation function
# @arguments further arguments are passed to aggregating function
# @arguments vector of variable names (can include "grand\_col" and "grand\_row") to create margins over
# @arguments logical vector to subset data set before reshaping
# @seealso \code{\link{reshape1}}
#X #Air quality example
#X names(airquality) <- tolower(names(airquality))
#X airquality.d <- deshape(airquality, id=c("month", "day"))
#X 
#X reshape(airquality.d, month ~ variable, mean)
#X reshape(airquality.d, month ~ variable, mean, margins=c("grand_row", "grand_col"))
#X reshape(airquality.d, day ~ month, mean, subset=variable=="ozone")
#X
#X #Chick weight example
#X names(ChickWeight) <- tolower(names(ChickWeight))
#X chick.d <- deshape(ChickWeight, id=2:4)
#X 
#X reshape(chick.d, time ~ variable, mean) # average effect of time
#X reshape(chick.d, diet ~ variable, mean) # average effect of diet
#X 
#X # How many chicks at each time? - checking for balance
#X reshape(chick.d, time ~ diet, length)
#X reshape(chick.d, chick ~ time, mean)
#X reshape(chick.d, chick ~ time, mean, subset=time < 10 & chick < 20)
#X 
#X reshape(chick.d, diet + chick ~ time)
#X reshape(chick.d, diet + chick ~ time, mean, margins="diet")
#X #Tips example
#X data(tips)
#X reshape(deshape(tips), sex ~ smoker, mean, subset=variable=="total_bill")
reshape <- function(data, formula = ... ~ variable, fun.aggregate, ..., margins, subset=TRUE) {
	subset <- eval(substitute(subset), data, parent.frame())  
	data <- data[subset, ]  
	variables <- reshape_parse_formula(formula, names(data))
	reshape1(data, variables$rows, variables$cols, fun.aggregate, margins=margins, ...)
}

# Reshaping workhorse.
# Takes data frame and variable list and reshapes data.
#
# @arguments data frame
# @arguments variables to appear in columns
# @arguments variables to appear in rows
# @arguments aggregation function
# @arguments vector of variable names (can include "grand\_col" and "grand\_row") to create margins over
# @arguments further arguments are passed to aggregating function
# @seealso \code{\link{reshape}}
# @keyword manip
reshape1 <- function(data, rows = NULL, cols = NULL, fun.aggregate, margins, ...) {

	variables <- clean.vars(c(rows, cols))
	aggregate <- nrow(unique(data[,variables, drop=FALSE])) < nrow(data)

	if (aggregate) {
		if (missing(fun.aggregate)) (stop("Aggregation requires fun.aggregate"))
		data.r <- expand(condense(data, variables, fun.aggregate, ...))		
	} else {
		data.r <- data[,c(variables, "value")]	
		data.r$results <- data.r$value
		data.r$value <- NULL		
	}

	if ("result_variable" %in% names(data.r) && !("result_variable" %in% c(rows,cols))) {
		cols = c(cols, "result_variable")
	}

	margins.all <- margin.vars(clean.vars(rows), clean.vars(cols), margins)
	if (length(margins.all) > 0) {
		if (missing(fun.aggregate)) (stop("Margins require fun.aggregate"))
		margins.r <- do.call("rbind.fill",lapply(margins.all, function(x) expand(condense(data, x, fun.aggregate, ...))))
		result <- rbind.fill(data.r, margins.r)
	} else {
		result <- data.r
	}
	rownames(result) <- 1:nrow(result)

	result <- result[do.call("order", result[,c(rows,cols), drop=FALSE]), ,drop=FALSE]
	# use fun.aggregate instead of "value"? 
	row.names <- if (!is.null(rows) && length(rows) > 0) unique(result[,rows,drop=FALSE]) else data.frame(value="value") 
	col.names <- if (!is.null(cols) && length(cols) > 0) unique(result[,cols,drop=FALSE]) else data.frame(value="value")

	# make sure all necessary cells exist
	all.combinations <- data.frame(expand.grid.df(result[,rows, drop=FALSE], result[,cols, drop=FALSE]))
	result <- merge_recurse(list(result, all.combinations))
	result <- result[do.call("order", result[,c(rows,cols), drop=FALSE]), ,drop=FALSE]

	reshaped <- matrix(unlist(result$result), nrow=nrow(row.names), ncol=nrow(col.names), byrow=TRUE)
	
	attr(reshaped, "r.aggregate") <- aggregate
	attr(reshaped, "r.col.names") <- col.names
	attr(reshaped, "r.row.names") <- row.names
	class(reshaped) <- c("reshape_matrix", "matrix")

	as.data.frame(reshaped)
}

# Parse formula for reshaping
#
# @value row character vector of row names
# @value col character vector of column names
# @value aggregate boolean whether aggregation will occur
# @keyword manip
reshape_parse_formula <- function(formula = ...  ~ variable, varnames) {
	check_formula(formula, varnames)
	
	rows <- all.vars(formula[[2]])
	cols <- all.vars(formula[[3]])
	
	# Remove . (placeholder since you can't have a formula like something ~ )
	rows <- rows[rows != "."]
	cols <- cols[cols != "."]

	remainder <- varnames[!(varnames %in% c(rows, cols, "value"))]
	if (any(rows == "...")) rows <- c(rows[rows != "..."], remainder)
	if (any(cols == "...")) cols <- c(cols[cols != "..."], remainder)
 	
	list(rows = rows, cols = cols)	
}

# Checks that formula is a valid reshaping formula.
#
# \enumerate{
#   \item expressions match known variables
#	 \item same variable not used on both sides
# }
# @keyword manip
check_formula <- function(formula, varnames) {
	if (!("formula" %in% class(formula))) stop("First argument must be a formula")
	if (length(formula) != 3)  stop("Formula must be of form row vars ~ col vars")
	vars <- all.vars(formula) 
	if (!all(vars %in% c(".", "...","result_variable",varnames))) stop("Formula contains variables not in list of known variables")
	if (length(unique(vars)) < length(vars)) stop("Variable names repeated")
}

# Merge together a series of data.frames
#
# Order of data frames should be from most complete to least complete (?- CHECK) 
#
# @arguments list of data frames to merge
# @seealso \code{\link{merge_recurse}}
# @keyword manip
merge_all <- function(dfs, ...) {
	if (length(dfs)==1) return(dfs[[1]])
	df <- merge_recurse(dfs, ...)
	df <- df[, match(names(dfs[[1]]), names(df))]
	df[do.call("order", df[, -ncol(df), drop=FALSE]), ,drop=FALSE]
}

# Recursively merge data frames
#
# @arguments list of data frames to merge
# @seealso \code{\link{merge_all}}
# @keyword manip
merge_recurse <- function(dfs, ...) {
	if (length(dfs) == 2) {
		merge(dfs[[1]], dfs[[2]], all=TRUE, sort=FALSE, ...)
	} else {
		merge(dfs[[1]], Recall(dfs[-1]), all=TRUE, sort=FALSE, ...)
	}
}