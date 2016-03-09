#' Log Loss Binary
#'
#' Calculates Log Loss Binary
#' y is the known response, yhat is the predicted
#' @param y, yhat
#' @keywords loss
#' @export
#' @examples
#' LogLossBinary()

LogLossBinary <- function(y, yhat, eps = 1e-15) {
	yhat = pmin(pmax(yhat, eps), 1-eps)
	- (sum(y * log(yhat) + (1 - y) * log(1 - yhat))) / length(y)
}
