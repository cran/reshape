# Deshape
# Deshape a data frame into form suitable for easy reshaping.
#
# Along with reshape, deshape is the only function from this package
# you will actually use.  The rest are all support functions for these two.
# Deshape gets your data into a form suitable for reshape to work on.
#
# You need to tell deshape which of your variables are id variables, and 
# which are measured variables.  For most practical uses, the id variables
# will be categorical, and the measured variables continuous.  
#
# Note that if you only supply one of id.var and measure.var, deshape
# will assume the remainder of the variables in the data set belong to 
# the other.  If you supply neither, deshape will assume integer and factor
# variables are id variables, and all other are measured.
# 
# @arguments data Data set to deshape
# @arguments id.var Identifying variables. If blank, will use all non measure.var variables
# @arguments measure.var Measured variables. If blank, will use all non id.var variables
# @value Deshaped data.frame
# @keyword manip
#X data(tips)
#X head(deshape(tips))
#X names(airquality) <- tolower(names(airquality))
#X airquality.d <- deshape(airquality, id=c("month", "day"))
#X head(airquality.d)
#X names(ChickWeight) <- tolower(names(ChickWeight))
#X chick.d <- deshape(ChickWeight, id=2:4)
deshape <- function(data, id.var, measure.var, variable_name = "variable") {
	var <- deshape.check(data, id.var, measure.var)

	df <- do.call("rbind", lapply(var$measure, function(x) {
		df <- data.frame(data[,var$id, drop=FALSE])
		df[,variable_name] <- x
		df$value <- (data[,x]) #as.list
		df
	}))
	
	df[complete.cases(df),]
}

# Deshape check.
# Check that input variables to deshape are appropriate.
#
# If id.var or measure.var are missing, deshape.check will do it's 
# best to impute them.
#
# @keyword manip
# @arguments variable names
# @arguments Vector of identifying variable names or indexes
# @arguments Vector of Measured variable names or indexes
# @value id list id variable names
# @value measure list of measured variable names
deshape.check <- function(data, id.var, measure.var) {
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
