# Print reshaped data frame
#
# @argument Reshaped data frame
# @argument Argument required to match generic
# @argument Argument required to match generic
# @keyword internal
print.cast_df <- function(x, digits=getOption("digits"), ..., colnames=TRUE) {
	unx <- x
	class(unx) <- "data.frame"
	label.rows <- names(rrownames(x))
	
	labels <- strip.dups(unx[,names(x) %in% label.rows, drop=FALSE])
	colnames(labels) <- label.rows[names(x) %in% label.rows]
	data <-   as.matrix(format(unx[,!(names(x) %in% label.rows), drop=FALSE]))
	
	col.labels <- t(strip.dups(rcolnames(x)))
	
	bottom <- cbind(labels,data)
	top <- cbind(matrix("", ncol=ncol(labels)-1, nrow=nrow(col.labels)), names(rcolnames(x)), col.labels)
	if(colnames) {
		middle <- colnames(bottom)
	} else {
		middle <- c(colnames(labels), rep("", ncol(bottom) - length(colnames(labels))))
	}

	result <- rbind(top, middle, bottom)
	rownames(result) <- rep("", nrow(result))
	colnames(result) <- rep("", ncol(result))

	print(result, quote=FALSE, right=TRUE)	
}

# Subset a cast data frame 
# Subset a cast data frame. This will result in a normal dataframe, because it is 
# very tricky to work out if you have kept all the "name" columns necessary
# for all the normal cast data.frame operations.
# 
# @arguments cast\_df object to subset
# @arguments row indices
# @arguments column indices
# @arguments other arguments
# @arguments discard extra dimensions?
# @keyword internal
"[.cast_df" <- function(x, i=1:nrow(x), j=1:ncol(x), ..., drop = FALSE) {
	unx <- x
	class(unx) <- "data.frame"
	
	unx[i, j, ..., drop=FALSE]
}

# Strip duplicates.
# Strips out duplicates from data.frame and replace them with periods.
# 
# @arguments data.frame to modify
# @value character matrix
# @keyword internal
strip.dups <- function(df) {
	clear.dup <- function(dups,ret=dups) ifelse(duplicated(dups), "", ret)

	mat <- apply(df, c(1,2), as.character)
	mat[is.na(mat)] <- "."
	do.call(cbind, lapply(1:ncol(mat), function(x) clear.dup(mat[,1:x, drop=FALSE], mat[,x, drop=FALSE])))
}


# Convert cast data frame into a matrix
#
# @arguments Reshape data frame
# @keyword internal
as.matrix.cast_df <- function(x) {
	m <- as.matrix.data.frame(x[, -(1:(ncol(rrownames(x)))), drop=FALSE])
	rdimnames(m) <- rdimnames(x)
	class(m) <- c("cast_matrix", "matrix")

	m
}
