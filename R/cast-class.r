# Print reshaped matrix.
#
# @arguments reshaped matrix
# @arguments number of digits to show
# @arguments other arguments to match generic
# @keyword manip
print.cast_matrix <- function(x, digits=getOption("digits"), ...) {
	xrv <- attr(x, "r.row.names")
	xcv <- attr(x, "r.col.names")


	col.labels <- cbind(
		matrix("", nr = length(xcv), nc = length(xrv)), 
		names(xcv),
		t(as.matrix(xcv))
	)

	col.labels[is.na(col.labels)] <- "."
	col.labels[1, duplicated(col.labels[1,])] <- ""


	row.labels <- rbind(
		names(xrv), 
		as.matrix(xrv)
	)
	row.labels[is.na(row.labels)] <- "."

	#row.labels[duplicated(row.labels[,1:2]),2] <- ""
	row.labels[duplicated(row.labels[,1]),1] <- ""

	data.borders <- cbind(
		rep("", nrow(row.labels)), 
		rbind(
			rep("", ncol(x)), 
			format(as.matrix(unclass(x)), digits=digits)
		)
	)

	mat <- format(rbind(
		col.labels, 
		cbind(row.labels, data.borders)
	))

	cat(t(mat), sep = c(rep(" ", ncol(mat) - 1), "\n"))
	invisible(mat)
}


# Print reshaped data frame
#
# @argument Reshaped data frame
# @argument Argument required to match generic
# @argument Argument required to match generic
# @keyword manip
print.cast_df <- function(x, digits=getOption("digits"), ...) {
	label.rows <- attr(x, "r.row.labels")
	
	labels <- as.matrix(format(x[,names(x) %in% label.rows, drop=FALSE]))
	data <-   as.matrix(format(x[,!(names(x) %in% label.rows), drop=FALSE]))
	
	labels[is.na(labels)] <- "."
	if (ncol(labels) > 3) labels[duplicated(labels[,1:3]),1:3] <- ""
	if (ncol(labels) > 2) labels[duplicated(labels[,1:2]),1:2] <- ""
	labels[duplicated(labels[,1]),1] <- ""
	
	result <- cbind(labels,data)
	rownames(result) <- rep("", nrow(result))

	print(result, quote=FALSE, right=TRUE)	
}


# Convert cast matrix into a data frame
#
# @argument Reshape matrix
# @argument Argument required to match generic
# @argument Argument required to match generic
# @keyword manip
as.data.frame.cast_matrix <- function(x, row.names, optional) {
	col.names <- attr(x, "r.col.names")
	row.names <- attr(x, "r.row.names")

	colnames(x) <- as.vector(apply(col.names, 1, function(x) paste(gsub("NA",".",paste(x)), collapse="_")))
	r.df <- data.frame(row.names, unclass(x))

	class(r.df) <- c("cast_df", "data.frame")
	attr(r.df, "r.aggregate") <- attr(x, "r.aggregate")
	attr(r.df, "r.col.names") <- attr(x, "r.col.names")
	attr(r.df, "r.row.labels") <- names(attr(x, "r.row.names"))

	r.df
}

# Convert cast data frame into a matrix
#
# @argument Reshape data frame
# @keyword manip
as.matrix.cast_df <- function(x) {
	row.names <- attr(x, "r.row.names")

	m <- as.matrix.data.frame(x[, -(1:(ncol(row.names))), drop=FALSE])
	attr(m, "r.aggregate") <- attr(x, "r.aggregate")
	attr(m, "r.col.names") <- attr(x, "r.col.names")
	attr(m, "r.row.names") <- attr(x, "r.row.names")
	class(m) <- c("cast_matrix", "matrix")

	m
}
