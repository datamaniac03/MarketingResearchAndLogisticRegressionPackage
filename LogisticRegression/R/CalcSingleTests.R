#' Print Single Results
#'
#' Prints the test results for single variables
#' xtrain and xtest are dataframes
#' @param model, xtrain, xtest
#' @keywords wald tests
#' @export
#' @examples
#' CalcSingleTests()

CalcSingleTests <- function(model, xtrain, xtest) {

	# Identify factors in input data
	w1 <- FactorID(xtrain)	
	w2 <- FactorID(xtest)	
	
	# Case 1: when there are no factors
	if(length(w1) == 0) {
		xtrainNom <- xtrain
		xtestNom <- xtrain
	}

	# Case 2: when all variables are factors
	if(length(w1) == length(xtrain[1,])) {
		xtrainNom <- xtrain
		xtestNom <- xtrain
	}

	# Case 3: when some are factors (and some are numerical)
	else if(length(w1) > 0) {
		xtrainNom <- xtrain[,w1]
		xtestNom <- xtest[,w2]
		xtrainNum <- xtrain[,-w1]
		xtestNum <- xtest[,-w2]
		xtrain <- cbind(xtrainCat, xtrainNum)
		xtest <- cbind(xtestCat, xtestNum)
	}

	# If there are factors, do the tests
	if(exists("xtrainNom") == TRUE) {
	cat(noquote("===================================\n"))
	cat(noquote("== Test Single Nominal Variables ==\n"))
	cat(noquote("===================================\n"))	
	SingleTestsNom(xtrainNom, model)
	}

	# If there are numerical, do the tests
	if(exists("xtrainNum") == TRUE) {
	cat(noquote("=====================================\n"))
	cat(noquote("== Test Single Numerical Variables ==\n"))
	cat(noquote("=====================================\n"))	
	SingleTestsNum(xtrainNum, model)
	}
}