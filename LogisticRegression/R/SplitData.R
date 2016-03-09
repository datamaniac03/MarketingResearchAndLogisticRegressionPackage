#' Split the data
#'
#' Randomly splits the data in half
#' @param X, Y
#' @keywords K-means segmentation
#' @export
#' @examples
#' SplitData()

SplitData <- function(X, Y){
	sub <- sample(length(Y), size = floor(length(Y) * 0.5))
	xtrain <- X[sub,]
	xtest  <- X[-sub,]
	ytrain <- factor(Y[sub])
	ytest  <- factor(Y[-sub])
	return(list(xtrain, ytrain, xtest, ytest))
}

