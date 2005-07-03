# Melt
# Melt a data frame into form suitable for easy casting.
#
# Along with \link{cast} and \link{recast}, melt is the only function from this package
# you will actually use.  The rest are all support functions for these two.
# melt gets your data into a suitable form (molten) for cast to work on.
#
# You need to tell melt which of your variables are id variables, and 
# which are measured variables.  For most practical uses, the id variables
# will be categorical, and the measured variables continuous.   If you only 
# supply one of id.var and measure.var, melt will assume the remainder of 
# the variables in the data set belong to the other.  If you supply neither, 
# melt will assume integer and factor  variables are id variables, 
# and all other are measured.
# 
# @arguments Data set to melt
# @arguments Identifying variables. If blank, will use all non measure.var variables
# @arguments Measured variables. If blank, will use all non id.var variables
# @arguments Name of the variable that will store the names of the original variables
# @arguments Should NAs be preserved or dropped from the data set?
# @value molten data
# @keyword manip
# @seealso \url{http://had.co.nz/reshape/}
#X data(tips)
#X head(melt(tips))
#X names(airquality) <- tolower(names(airquality))
#X airquality.d <- melt(airquality, id=c("month", "day"))
#X head(airquality.d)
#X names(ChickWeight) <- tolower(names(ChickWeight))
#X chick.d <- melt(ChickWeight, id=2:4)
melt <- function(data, id.var, measure.var, variable_name = "variable", preserve.na = TRUE) {
	remove.na <- function(df) if (preserve.na) df else df[complete.cases(df),,drop=FALSE]

	var <- melt.check(data, id.var, measure.var)
	
	if (length(var$measure) == 0) {
		return(remove.na(data[,var$id, drop=FALSE]))
	}
	
	df <- do.call("rbind", lapply(var$measure, function(x) {
		df <- data.frame(data[,var$id, drop=FALSE])
		df[,variable_name] <- x
		df$value <- (data[,x]) #as.list
		df
	}))
	remove.na(df)
}

# Melt check.
# Check that input variables to melt are appropriate.
#
# If id.var or measure.var are missing, melt.check will do its 
# best to impute them.If you only 
# supply one of id.var and measure.var, melt will assume the remainder of 
# the variables in the data set belong to the other.  If you supply neither, 
# melt will assume integer and factor  variables are id variables, 
# and all other are measured. 
#
# @keyword manip
# @arguments data frame
# @arguments Vector of identifying variable names or indexes
# @arguments Vector of Measured variable names or indexes
# @value id list id variable names
# @value measure list of measured variable names
melt.check <- function(data, id.var, measure.var) {
	varnames <- names(data)
	if (!missing(id.var) && is.numeric(id.var)) id.var <- varnames[id.var]
	if (!missing(measure.var) && is.numeric(measure.var)) measure.var <- varnames[measure.var]

	if (!missing(id.var) && !(id.var %in% varnames)) {stop("Unknown id variable names")}
	if (!missing(measure.var) && !(measure.var %in% varnames)) {stop("Unknown measured variable names")}

	if (missing(id.var) && missing(measure.var)) {
		categorical <- sapply(data, function(x) class(x)[1]) %in% c("factor", "ordered", "integer")
		id.var <- varnames[categorical]
		measure.var <- varnames[!categorical]
	}	

	if (missing(id.var)) id.var <- varnames[!(varnames %in% c(measure.var))]
	if (missing(measure.var)) measure.var <- varnames[!(varnames %in% c(id.var))]
	
	list(id = id.var, measure = measure.var)	
}
