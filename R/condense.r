# Condense
# Condense a data frame.
#
# Works very much like by, but keeps data in original data frame format.
# Results column is a list, so that each cell may contain an object or a vector etc.
# Assumes data is in molten format. Aggregating function must return the
# same number of arguments for all input.
#
# @arguments data frame
# @arguments variables to condense over
# @arguments aggregating function, may multiple values
# @arguments further arguments passed on to aggregating function
# @keyword manip
condense <- function(data, variables, fun, ...) {
	if (length(variables) == 0 ) {
		df <- data.frame(results = 0)
		df$results <- list(fun(data$value, ...))
		return(df)
	}

	sorted <- sort.df(data, variables)[,c(variables, "value"), drop=FALSE]
	duplicates <- duplicated(sorted[,variables, drop=FALSE])
	index <- cumsum(!duplicates)

	results <- tapply(sorted$value, index, fun, ..., simplify = FALSE)

	cols <- sorted[!duplicates,variables, drop=FALSE]
	cols$results <- array(results)
	cols
}

# expand
# Expand out condensed data frame.
#
# If aggregating function supplied to condense returns multiple values, this
# function "melts" it again, creating a new column called result\_variable.
#
# If the aggregating funtion is a named vector, then those names will be used,
# otherwise will be number X1, X2, ..., Xn etc.
#
# @arguments condensed data frame
# @keyword manip
expand <- function(data) {
	lengths <- unique(sapply(data$results, length))
	if (lengths == 1) return(data)

	first <- data[1, "results"][[1]]
	exp <- lapply(1:length(first), function(x) as.vector(unlist(lapply(data$results, "[", x))))
	names(exp) <- if (is.null(names(first))) make.names(1:length(first)) else names(first)

	x <- melt(data.frame(data[, 1:(ncol(data)-1), drop=FALSE], exp), m=names(exp),variable_name="result_variable", preserve.na = TRUE)
	colnames(x)[match("value", colnames(x), FALSE)] <- "result"
	x
}


# Clean variables.
# Clean variable list for reshaping.
#
# @arguments vector of variable names
# @value Vector of "real" variable names (excluding result\_variable etc.)
# @keyword manip
clean.vars <- function(vars) {vars[vars != "result_variable"]}

# Margin variables
# Works out list of variables to margin over to get desired margins.
#
# Variables that can't be margined over are dropped silently.
#
# @arguments column variables
# @arguments row variables
# @arguments vector of variable names to margin over.
# @keyword manip
margin.vars <- function(rows = NULL, cols = NULL, margins = NULL) {
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
	
	if (grand.row) margins.all <- c(margins.all, list(cols))
	if (grand.col)	margins.all <- c(margins.all, list(rows))
	if (grand.col && grand.row) margins.all <- c(margins.all, list(numeric(0)))
	
	duplicates <- duplicated(lapply(lapply(margins.all,function(x) if(!is.null(x)) sort(x)), paste, collapse=""))
	
	margins.all[!duplicates]
}

# expand grid
# expand grid of data frames
#
# Creates new data frame containing all combination of rows from df1 and df2.
#
# @arguments data frame 1 (varies fastest)
# @arguments data frame 2
# @keyword manip
expand.grid.df <- function(df1, df2, unique=TRUE) {
	u1 <- if (unique) unique(df1) else df1
	index1 <- 1:nrow(u1)

	u2 <- if (unique) unique(df2) else df2
	index2 <- 1:nrow(u2)

	if (ncol(df1) == 0) return(u2)
	if (ncol(df2) == 0) return(u1)

	grid <- expand.grid(index1, index2)
	df <- data.frame(u1[grid[,1],], u2[grid[,2],])
	colnames(df) <- c(colnames(df1), colnames(df2))
	df
}


# Rbind fill
# Rbind a list of data frames filling missing columns with NA 
#
# @arguments data frames to row bind together
# @keyword manip
rbind.fill <- function(...) {
	dfs <- list(...)
	if (length(dfs) == 0) return(list())

	all.names <- unique(unlist(lapply(dfs, names)))
	do.call("rbind", lapply(dfs, function(df) {
		if (length(df) == 0 || nrow(df) == 0) return(df)
 		missing.vars <- setdiff(all.names, names(df))
		if (length(missing.vars) > 0) df[, missing.vars] <- NA
		df
	}))
}