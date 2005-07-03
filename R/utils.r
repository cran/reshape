# Compact list
# Remove all NULL entries from a list
# 
# @arguments list
# @keyword manip 
compact <- function(l) {
  l[!sapply(l, is.null)]
}

# Defaults
# Convience method for combining a list of values with their defaults.
# 
# @arguments list of values
# @arguments defaults
# @keyword manip 
defaults <- function(x, y)  {
	c(x, y[setdiff(names(y), names(x))])
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


# Untable a dataset
# Given a tabulated dataset (or matrix) this will untabulate it
# by repeating each row by the number of times it was repeated
# 
# @arguments matrix or data.frame to untable
# @arguments vector of counts (of same length as \code{df})
# @keyword manip 
untable <- function(df, num) {
	df[rep(1:nrow(df), num), ]
}



# Unique default
# Convenience function for setting default if not unique
# 
# @arguments vector of values
# @arguments default to use if values not uniquez
# @keyword manip 
uniquedefault <- function(values, default) {
	unq <- unique(values)
	if (length(unq) == 1) unq[1] else "black"
}

# Rename
# Rename an object
# 
# @arguments object to be renamed
# @arguments named list specifying new names
# @keyword internal 
rename <- function(x, replace) {
	replacement <-  replace[names(x)]
	names(x)[!is.na(replacement)] <- replacement[!is.na(replacement)]
	x
}

# Round any
# Round to multiple of any number
# 
# @arguments numeric vector to round
# @arguments number to round to
# @arguments function to use for round (eg. \code{\link{floor}})
# @keyword internal 
round_any <- function(x, round, f=floor) {
	f(x / round) * round
}


# Update list
# Update a list, but don't create new entries
# 
# @arguments list to be updated
# @arguments list with updated values
# @keyword internal 
updatelist <- function(x, y)  {
	common <- intersect(names(x),names(y))
	x[common] <- y[common]
	x
} 


# Dimensions
# Number of dimensions of an array or vector
# 
# @arguments array 
# @keyword internal
dims <- function(x) length(vdim(x))


# Dimensions
# Compute dimensions for a vector similarly to arrays
# 
# @arguments array or vector
# @keyword internal 
vdim <- function(x) if (is.vector(x)) length(x) else dim(x)
