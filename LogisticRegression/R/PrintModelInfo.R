#' Print Model Info
#'
#' Prints model information
#' model is a model object, xtest is a data frame, ytest is a factor with two levels
#' @param model, xtest, ytest
#' @keywords model
#' @export
#' @examples
#' PrintModelInfo()

PrintModelInfo <- function(model, xtest, ytest) {
	# Summarize the model
	cat(noquote("=================================\n"))
	cat(noquote("== Logistic Regression Summary ==\n"))
	cat(noquote("=================================\n"))
	print(summary(model))

	# Do the full model vs. null model lrt	
	cat(noquote("=====================================================\n"))
	cat(noquote("== Full Model vs. Null Model Likelihood Ratio Test ==\n"))
	cat(noquote("=====================================================\n"))

	# Calculate test statistic
	cat(noquote("null deviance - deviance: "))
	cat(with(model, null.deviance - deviance))
	cat(noquote("\n"))

	# Print degrees of freedom for test	
	cat(noquote("degrees of freedom: "))
	cat(with(model, df.null - df.residual))
	cat(noquote("\n"))

	# Get p-value
	cat(noquote("p-value : "))
	pval <- with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
	cat(pval)
	cat(noquote("\n"))

	# Get log-likelihood
	cat(noquote("log-likelihood: "))
	llm <- logLik(model)
	cat(llm)
	cat(noquote("\n"))
	cat(noquote("\n"))

	# Note about p-value
	cat(noquote("Note: A p-value of < .05 (or some chosen alpha)\n")) 
	cat(noquote("indicates that the full model is better than the null.\n"))
	cat(noquote("\n"))

	# Get the analysis of deviance
	cat(noquote("==========================\n"))
	cat(noquote("== Analysis of Deviance ==\n"))
	cat(noquote("==========================\n"))
	print(anova(model, test="Chisq"))
	cat(noquote("\n"))

	# Get confidence intervals for model coefficients
	cat(noquote("===========================================\n"))
	cat(noquote("== Confidence Intervals for Coefficients ==\n"))
	cat(noquote("===========================================\n"))
	cat(noquote("= Profiled Log-Likelihood CIs =\n"))
	print(confint(model))
	cat(noquote("\n"))
	cat(noquote("= Standard Error CIs =\n"))
	print(confint.default(model))
	cat(noquote("\n"))

	# Get odds ratios and their confidence intervals
	cat(noquote("=============================\n"))
	cat(noquote("== Odds Ratios and 95% CIs ==\n"))
	cat(noquote("=============================\n"))
	print(exp(cbind(OR = coef(model), confint(model))))
	cat(noquote("\n"))
	
	# Calculate the tests for single variables
	CalcSingleTests(model, xtrain, xtest)
}
