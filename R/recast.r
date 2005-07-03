# Recast 
# \link{melt} and \link{cast} data in a single step
# 
# This conveniently wraps melting and casting a data frame into
# one step. 
#
# Todo: take advantage of reshape formula to reduce number of 
# columns in molten data frame to save time/spcae.
# 
# @arguments Data set to melt
# @arguments Casting formula, see \link{cast} for specifics
# @arguments Other arguments passed to \link{cast}
# @arguments Identifying variables. If blank, will use all non measure.var variables
# @arguments Measured variables. If blank, will use all non id.var variables
# @keyword manip
# @seealso \url{http://had.co.nz/reshape/}
recast <- function(data, formula, ..., id.var, measure.var) {
	var <- cast_parse_formula(formula, names(data))

	molten <- melt(data, id.var, measure.var)
	cast(molten, formula, ...)
}
