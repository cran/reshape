# Cast function
# Cast a molten data frame into the reshaped or aggregated form you want
#
# Along with \code{\link{melt}}  and \link{recast}, this is the only function you should ever need to use.
# Once you have melted your data, cast will arrange it into the form you desire
# based on the specification given by \code{formula}.
#
# The cast formula has the following format: \code{row_variable_1 + row_2 ~ col_variable + col_2}
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
# @seealso \code{\link{reshape1}}, \url{http://had.co.nz/reshape/}
#X #Air quality example
#X names(airquality) <- tolower(names(airquality))
#X airquality.d <- melt(airquality, id=c("month", "day"))
#X 
#X cast(airquality.d, month ~ variable, mean)
#X cast(airquality.d, month ~ variable, mean, margins=c("grand_row", "grand_col"))
#X cast(airquality.d, day ~ month, mean, subset=variable=="ozone")
#X cast(airquality.d, month ~ variable, range)
#X cast(airquality.d, month ~ variable + result_variable, range)
#X
#X #Chick weight example
#X names(ChickWeight) <- tolower(names(ChickWeight))
#X chick.d <- melt(ChickWeight, id=2:4)
#X 
#X cast(chick.d, time ~ variable, mean) # average effect of time
#X cast(chick.d, diet ~ variable, mean) # average effect of diet
#X 
#X # How many chicks at each time? - checking for balance
#X cast(chick.d, time ~ diet, length)
#X cast(chick.d, chick ~ time, mean)
#X cast(chick.d, chick ~ time, mean, subset=time < 10 & chick < 20)
#X 
#X cast(chick.d, diet + chick ~ time)
#X cast(chick.d, diet + chick ~ time, mean, margins="diet")
#X #Tips example
#X data(tips)
#X cast(melt(tips), sex ~ smoker, mean, subset=variable=="total_bill")
#X 
#X data(french_fries)
#X ff_d <- melt(french_fries, id=1:4)
#X cast(ff_d, subject ~ time, length)
#X cast(ff_d, subject ~ time, function(x) 30 - length(x))
#X cast(ff_d, variable ~ ., function(x) c(min=min(x), max=max(x)))
#X cast(ff_d, treatment ~ variable, mean, margins=c("grand_col", "grand_row"))
#X cast(ff_d, treatment + subject ~ variable, mean, margins="treatment", subset=subject < 5)
#X lattice::xyplot(X1 ~ X2 | variable, cast(ff_d, ... ~ rep), aspect="iso")
cast <- function(data, formula = ... ~ variable, fun.aggregate, ..., margins, subset=TRUE) {
	subset <- eval(substitute(subset), data, parent.frame())  
	data <- data[subset, ]  
	variables <- cast_parse_formula(formula, names(data))
	reshape1(data, variables$rows, variables$cols, fun.aggregate, margins=margins, ...)
}

# Casting workhorse.
# Takes data frame and variable list and casts data.
#
# @arguments data frame
# @arguments variables to appear in columns
# @arguments variables to appear in rows
# @arguments aggregation function
# @arguments vector of variable names (can include "grand\_col" and "grand\_row") to compute margins for, or TRUE to computer all margins
# @arguments further arguments are passed to aggregating function
# @seealso \code{\link{cast}}
# @keyword manip
#X names(airquality) <- tolower(names(airquality))
#X airquality.d <- melt(airquality, id=c("month", "day"))
#X #Basic call
#X reshape1(airquality.d, c("month"), , mean)
#X reshape1(airquality.d, c("month"), "variable", mean)
#X reshape1(airquality.d, c("day"), "month", mean)
#X 
#X #Explore margins
#X reshape1(airquality.d, c("month"), , mean, "month")
#X reshape1(airquality.d, c("month"), , mean, "grand_col")
#X reshape1(airquality.d, c("month"), , mean, "grand_row")
#X 
#X reshape1(airquality.d, c("month", "day"), , mean, "month")
#X reshape1(airquality.d, c("month"), "variable", mean, "month")
#X reshape1(airquality.d, c("variable"), "month", mean, "month")
#X reshape1(airquality.d, c("month"), "variable", mean, c("month","variable"))
#X 
#X reshape1(airquality.d, c("month"), "variable", mean, c("grand_row"))
#X reshape1(airquality.d, c("month"), "variable", mean, c("grand_col"))
#X reshape1(airquality.d, c("month"), "variable", mean, c("grand_row","grand_col"))
#X 
#X reshape1(airquality.d, c("variable","day"),"month", mean,c("variable"))
#X reshape1(airquality.d, c("variable","day"),"month", mean,c("variable","grand_row"))
#X reshape1(airquality.d, c("month","day"), "variable", mean, "month") 
#X 
#X # Multiple fnction returns
#X reshape1(airquality.d, c("month", "result_variable"), , range) 
#X reshape1(airquality.d, c("month"),"result_variable" , range) 
#X reshape1(airquality.d, c("result_variable", "month"), , range) 
#X 
#X reshape1(airquality.d, c("month", "result_variable"), "variable", range, "month")
#X reshape1(airquality.d, c("month", "result_variable"), "variable", range, "variable")
#X reshape1(airquality.d, c("month", "result_variable"), "variable", range, c("variable","month"))
#X reshape1(airquality.d, c("month", "result_variable"), "variable", range, c("grand_col"))
#X reshape1(airquality.d, c("month", "result_variable"), "variable", range, c("grand_row"))
#X 
#X reshape1(airquality.d, c("month"), c("variable"), function(x) diff(range(x))) 
reshape1 <- function(data, rows = NULL, cols = NULL, fun.aggregate, margins, ...) {
	cols.clean <- clean.vars(cols)
	rows.clean <- clean.vars(rows)
	variables <- c(rows.clean, cols.clean)

	if (!missing(margins) && isTRUE(margins)) margins <- c(variables, "grand_row", "grand_col")
	
	aggregate <- nrow(unique(data[,variables, drop=FALSE])) < nrow(data)
	if (aggregate) {
		if (missing(fun.aggregate)) {
			warning("Aggregation requires fun.aggregate: length used as default", call.=FALSE)
			fun.aggregate <- length
		}
		data.r <- expand(condense(data, variables, fun.aggregate, ...))

		if ("result_variable" %in% names(data.r) && !("result_variable" %in% c(rows,cols))) {
			cols <- c(cols, "result_variable")
		}
	} else {
		data.r <- data.frame(data[,c(variables)], result = data$value)	
	}

	margins.r <- compute.margins(data, margin.vars(rows.clean, cols.clean, margins), fun.aggregate, ...)
	result <- sort.df(rbind.fill(data.r, margins.r), c(rows,cols))
	result <- add.all.combinations(result, rows, cols)

	dim.names <- function(data, vars) {
		if (!is.null(vars) && length(vars) > 0) {
			unique(data[,vars,drop=FALSE]) 
		} else {
			data.frame(value="value") # use fun.aggregate instead of "value"? 
		}
	}
	row.names <- dim.names(result, rows)
	col.names <- dim.names(result, cols)

	reshaped <- matrix(unlist(result$result), nrow=nrow(row.names), ncol=nrow(col.names), byrow=TRUE)
	
	rownames(reshaped) <- 1:nrow(reshaped)
	
	as.data.frame(cast_matrix(reshaped, list(row.names, col.names)))
}

sort.df <- function(data, vars) {
	data[do.call("order", data[,vars, drop=FALSE]), ,drop=FALSE]
}


# Compute margins
# Compute marginal values.
# 
# @arguments data frame
# @arguments margins to compute
# @arguments aggregation function
# @arguments other argument passed to aggregation function
# @keyword manip 
compute.margins <- function(data, margins, fun.aggregate, ...) {
	if (length(margins) == 0) return(data.frame())
	
	if (missing(fun.aggregate)) {
		warning("Margins require fun.aggregate: length used as default", call.=FALSE)
		fun.aggregate <- length
	}
	
	do.call("rbind.fill",lapply(margins, function(x) expand(condense(data, x, fun.aggregate, ...))))
}

# Add all combinations
# Add all combinations of the given rows and columsn to the data frames.
# 
# This function is used to ensure that we have a matrix of the appropriate
# dimensionaliy with no missing cells.
# 
# @arguments data.frame
# @arguments row variables (character vector)
# @arguments column variables (character vector)
# @keyword manip 
add.all.combinations <- function(data, rows, cols) {
	if (length(rows) + length(cols) == 0) return(data)
	all.combinations <- data.frame(expand.grid.df(data[,rows, drop=FALSE], data[,cols, drop=FALSE]))
	result <- merge_recurse(list(data, all.combinations)) 
	
	sort.df(result, c(rows,cols))
}

# Parse formula for casting
#
# @value row character vector of row names
# @value col character vector of column names
# @value aggregate boolean whether aggregation will occur
# @keyword manip
cast_parse_formula <- function(formula = ...  ~ variable, varnames) {
	check_formula(formula, varnames)
		
	rows <- all.vars(formula[[2]])
	cols <- all.vars(formula[[3]])
	
	# Remove . (placeholder since you can't have a formula like y ~ )
	rows <- rows[rows != "."]
	cols <- cols[cols != "."]

	remainder <- varnames[!(varnames %in% c(rows, cols, "value"))]
	if (any(rows == "...")) rows <- c(rows[rows != "..."], remainder)
	if (any(cols == "...")) cols <- c(cols[cols != "..."], remainder)

	list(rows = rows, cols = cols)	
}

# Parse formula
# Parse an R formula into convenient chucks
# 
# @arguments formula to parse
# @arguments variable names
# @keyword manip 
parse_formula <- function(formula, varnames) {
	split.vars <- function(x) strsplit(x, "[*+]")
	tidy_var <- function(x) lapply(x, function(x) unlist(replace(x, x == "...", list(remainder))))

	formula <- strsplit(gsub("\\s+","", formula), "\\|")[[1]]
	
	list.vars  <- unlist(split.vars(formula[2]))
	array.vars <- split.vars(strsplit(formula[1], "~")[[1]])
	all.vars <- unlist(c(list.vars, array.vars))

	if (length(unique(all.vars)) < length(all.vars)) stop("Variable names repeated")
	remainder <- varnames[!(varnames %in% c(all.vars, "value"))]
	if (!all(all.vars %in% c(".", "...","result_variable",varnames))) stop("Formula contains variables not in list of known variables")

	list(list=tidy_var(list(list.vars))[[1]], array=tidy_var(array.vars))#, all=all.vars)
}

# Checks that formula is a valid reshaping formula.
#
# \enumerate{
#   \item expressions match known variables
#	 \item same variable not used on both sides
# }
# @arguments formula to check
# @arguments vector of variable names
# @keyword manip
check_formula <- function(formula, varnames) {
	if (!("formula" %in% class(formula))) stop("First argument must be a formula")
	#if (length(formula) != 3)  stop("Formula must be of form row vars ~ col vars")
	vars <- all.vars(formula) 
	if (!all(vars %in% c(".", "...","result_variable",varnames))) stop("Formula contains variables not in list of known variables")
	if (length(unique(vars)) < length(vars)) stop("Variable names repeated")
}

# Merge together a series of data.frames
#
# Order of data frames should be from most complete to least complete 
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