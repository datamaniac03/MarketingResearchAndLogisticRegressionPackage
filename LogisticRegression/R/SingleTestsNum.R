#' Wald tests for Single Numerical Variables
#'
#' Conducts Wald tests on single numerical variables
#' @param x, model A data frame and a model
#' @keywords wald tests
#' @export
#' @examples
#' SingleTestsNum()

SingleTestsNum <- function(x, model) {
	N <- ncol(x)
	for(i in 1:N) {
			name <- colnames(x)[i]
			cat(noquote("====================================\n"))
			cat(paste(name, "\n"))
			wtest <- wald.test(b = coef(model), Sigma = vcov(model), Terms = i)
	}
	print(wtest)
	cat("\n")		
}
