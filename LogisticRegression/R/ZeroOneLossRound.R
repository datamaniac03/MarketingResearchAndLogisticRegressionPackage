#' Rounded 0-1 Error
#'
#' Calculates 0-1 Error after rounding predictions
#' @param y, yhat
#' @keywords loss
#' @export
#' @examples
#' ZeroOneLossRound()

ZeroOneLossRound <- function(y, yhat) {
	yhat <- round(yhat)
	y <- as.numeric(y) - 1
	Loss <- sum(abs(y - yhat)) / length(y)
	return(as.vector(t(Loss)))
}