# Stamp 
# Stamp is like reshape but the "stamping" function is passed the entire data frame, instead of just a few variables.
# 
# It is very similar to the \code{\link{by}} function except in the form
# of the output which is arranged using the formula as in \code{\link{reshape}}
# 
# Note that it's very easy to create objects that R can't print with this
# function.  You will probably want to save the results to a variable and
# then use extract the results.  See the examples.
# 
# @arguments data.frame (no molten)
# @arguments formula that describes arrangement of result, columns ~ rows, see \code{\link{reshape}} for more information
# @arguments aggregation function to use, should take a data frame as the first argument
# @arguments arguments passed to the aggregation function
# @arguments margins to compute (character vector, or \code{TRUE} for all margins), can contain \code{grand_row} or \code{grand_col} to inclue grand row or column margins respectively.
# @arguments logical vector by which to subset the data frame, evaluated in the context of the data frame so you can 
#@keyword manip 
#X french_fries$time <- as.numeric(as.character(french_fries$time))
#X stamp(french_fries, subject ~ ., function(df) coef(lm(painty ~ time, df))[2])
#X stamp(french_fries, subject ~ treatment, function(df) coef(lm(painty ~ time, df))[2])
#X models <- stamp(french_fries, subject ~ ., function(df) lm(painty ~ time, df))
#X dim(models)
#X anova(models[[3,1]])
stamp <- function(data, formula = . ~ ., fun.aggregate, ..., margins=NULL, subset=TRUE) {
	# Can we think of stamp in a simpler way?
	# If one part creates an array with the correct data in each cell, then the second
	# part would only need to run apply.

	subset <- eval(substitute(subset), data, parent.frame())  
	if (is.null(formula)) return(fun.aggregate(data))
	data <- data[subset, ]  
	variables <- cast_parse_formula(formula, names(data))
	stamp.work(data, variables$rows, variables$cols, fun.aggregate, margins=margins, ...)
}

# Condense a data frame
# 
# @arguments data frame
# @arguments character vector of variables to condense over
# @arguments function to condense with
# @arguments arguments passed to condensing function
# @keyword manip 
condense.df <- function(data, variables, fun, ...) {
	if (length(variables) == 0 ) {
		df <- data.frame(results = 0)
		df$results <- list(fun(data, ...))
		return(df)
	}

	sorted <- sort.df(data, variables)
	duplicates <- duplicated(sorted[,variables, drop=FALSE])
	index <- cumsum(!duplicates)

	results <- by(sorted, index, fun, ...)

	cols <- sorted[!duplicates,variables, drop=FALSE]
	cols$results <- array(results)
	cols
}

# Sort data frame
# Convenience method for sorting a data frame using the given variables.
# 
# @arguments data frame to sort
# @arguments variables to use for sorting
# @returns sorted data frame
# @keyword manip 
sort.df <- function(data, vars=names(data)) {
	if (length(vars) == 0) return(data)
	data[do.call("order", data[,vars, drop=FALSE]), ,drop=FALSE]
}

# Stamp work
# This is the workhouse that powers the stamp function.
# 
# You shouldn't call this function, please use \code{\link{stamp}}
# instead.
# 
# @arguments data frame
# @arguments row variables (character vector)
# @arguments column variables (character vector)
# @arguments stamping function, should take a dataframe as it's first arugment
# @arguments variables to margin over (character vector, or TRUE, for all margins)
# @arguments arguments passed to stamping function
# @keyword manip 
stamp.work <- function(data, rows = NULL, cols = NULL, fun, margins=NULL, ...) {
	variables <- c(rows, cols)
	if (!missing(margins) && isTRUE(margins)) margins <- c(variables, "grand_row", "grand_col")
	
	data.r <- condense.df(data, variables, fun, ...)
	
	if (length(margins) > 0) {
		margins.all <- margin.vars(rows, cols, margins)
		#browser()
		margins.r <- do.call("rbind.fill",lapply(margins.all, function(x) condense.df(data, x, fun, ...)))
		data.r <- rbind.fill(data.r, margins.r)
	}
	
	result <- sort.df(data.r, c(rows,cols))
	result <- add.all.combinations(result, rows, cols)

	row.names <- dim.names(result, rows)
	col.names <- dim.names(result, cols)

	reshaped <- matrix(result$result, nrow=nrow(row.names), ncol=nrow(col.names), byrow=TRUE)
	
	rownames(reshaped) <- 1:nrow(reshaped)
	#attr(reshaped, "r.col.names") <- col.names
	#attr(reshaped, "r.row.names") <- row.names
	cast_matrix(reshaped, list(row.names, col.names))
	#reshaped
}

# Dimension names
# Convenience method for extracting row and column names 
# 
# @arguments data frame
# @arguments variables to use
# @keyword manip 
dim.names <- function(data, vars) {
	if (!is.null(vars) && length(vars) > 0) {
		unique(data[,vars,drop=FALSE]) 
	} else {
		data.frame(value="value") # use fun.aggregate instead of "value"? 
	}
}
