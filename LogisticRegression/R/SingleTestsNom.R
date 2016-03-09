#' Wald tests for Single Categorical Variables
#'
#' Conducts Wald tests on single categorical variables
#' x is a data frame, model is a model object
#' @param x, model A data frame and a model
#' @keywords wald tests
#' @export
#' @examples
#' SingleTestsNom()

SingleTestsNom <- function(x, model) {
	nLevels <- numLvls(x)
	N <- ncol(x)
	for(i in 1:N) {
		coeffIDXlb <- 1 + sum(nLevels[1:(i-1)])
		coeffIDXub <- coeffIDXlb + sum(nLevels[i])
		name <- colnames(x)[i]
		cat(noquote("==================================\n"))
		cat(paste(name, "\n"))
		wtest <- wald.test(b = coef(model), Sigma = vcov(model), Terms = coeffIDXlb:coeffIDXub)
		print(wtest)
		cat("\n")		
	}
}