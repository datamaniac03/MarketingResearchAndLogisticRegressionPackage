#' Convert character columns into factors
#'
#' This function converts character columns to factors
#' x is a data frame
#' @param x A data frame
#' @keywords factors
#' @export
#' @examples
#' Char2Factors()

Char2Factors <- function(x) {
	for(i in 1:length(x[1,])) {
	x[,i] <- factor(x[,i], exclude=NULL)
	}
	return(x)
}