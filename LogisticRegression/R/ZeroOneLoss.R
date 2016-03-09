#' 0-1 Error
#'
#' Calculates 0-1 Error 
#' @param y, yhat
#' @keywords loss
#' @export
#' @examples
#' ZeroOneLoss()

ZeroOneLoss <- function(y, yhat) {
	y <- as.numeric(y) - 1
	Loss <- sum(abs(y-yhat)) / length(y)
	return(as.vector(t(Loss)))
}
