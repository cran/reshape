# Cast matrix.
# Createa a new cast matrix
# 
# @arguments matrix to turn into cast matrix
# @arguments list of dimension names (as data.frames), row, col, ...
# @value object of type \code{\link{cast_matrix}}
# @keyword internal
cast_matrix <- function(m, dimnames) {
	rdimnames(m) <- dimnames
	class(m) <- c("cast_matrix", class(m))
	m
}

# Dimension names
# These methods provide easy access to the special dimension names
# associated without the output of reshape
# 
# @alias rdimnames<- 
# @alias rcolnames 
# @alias rcolnames<- 
# @alias rrownames 
# @alias rrownames<- 
# @keyword internal
rdimnames <- function(x) attr(x, "rdimnames")
"rdimnames<-" <- function(x, value) {
	attr(x, "rdimnames") <- value
	x
}
rcolnames <- function(x) rdimnames(x)[[2]]
"rcolnames<-" <- function(x, value) {
	dn <- rdimnames(x)
	dn[[2]] <- value
	rdimnames(x) <- dn
	x
}
rrownames <- function(x) rdimnames(x)[[1]]
"rrownames<-" <- function(x, value) {
	dn <- rdimnames(x)
	dn[[1]] <- value
	rdimnames(x) <- dn
	x
}


# Print reshaped matrix.
#
# @arguments reshaped matrix
# @arguments number of digits to show
# @arguments other arguments to match generic
# @keyword internal
print.cast_matrix <- function(x, digits=getOption("digits"), ...) {
	print(as.data.frame(x), digits=digits, ..., colnames=FALSE)
}


# Convert cast matrix into a data frame
#
# @argument Reshape matrix
# @argument Argument required to match generic
# @argument Argument required to match generic
# @keyword internal
as.data.frame.cast_matrix <- function(x, row.names, optional, ...) {
	unx <- unclass(x)
	colnames(unx) <- gsub("NA", ".", apply(rcolnames(x), 1, paste, collapse="_"))
	
	r.df <- data.frame(rrownames(x), unx)
	class(r.df) <- c("cast_df", "cast", "data.frame")
	rdimnames(r.df) <- rdimnames(x)

	r.df
}

# Subset a cast matrix
# Subset a cast matrix just like you would a normal matrix.  Preserves row and column names nicely.
# 
# @arguments cast matrix object
# @arguments row indices
# @arguments col indices
# @arguments other arguments not used
# @arguments discard extra dimensions?
# @keyword internal
"[.cast_matrix" <- function(x, i=1:nrow(x), j=1:ncol(x), ..., drop=FALSE) {
	unx <- unclass(x)
	cast_matrix(unx[i, j, ..., drop=FALSE], list(rrownames(x)[i, , drop=FALSE], rcolnames(x)[j,, drop=FALSE]))
}