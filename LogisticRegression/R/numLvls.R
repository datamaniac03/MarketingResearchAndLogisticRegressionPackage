#' Count number of factor levels
#'
#' Calculates the number of levels in a categorical variable and subtracts 1
#' x is a data frame
#' @param x A data frame
#' @keywords factors
#' @export
#' @examples
#' numLvls()

numLvls <- function(x) {
	N <- length(x)
	nlvls <- rep(0,length(x))
	for(i in 1:length(x)) {
		nlvls[i] <- nlevels(x[,i])-1
	}
	return(nlvls)
}