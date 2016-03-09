#' Runs variable selection procedures
#'
#' Runs 5 types of variable selection and prints to console
#' @param model A model
#' @keywords variable selection
#' @export
#' @examples
#' VarSelection()

VarSelection <- function(model) {

	# drop all possible single terms in the model
	cat(noquote("==================================================================\n"))
	cat(noquote("== Model Selection: Drop all Possible Single Terms in the Model ==\n"))
	cat(noquote("==================================================================\n"))
	print(drop1(model, test="Chisq"))
	cat(noquote("\n"))
	
	# drop all possible single terms in the model
	cat(noquote("====================================================================\n"))
	cat(noquote("== Model Selection: Add Second Order Interaction Terms the Model ===\n"))
	cat(noquote("====================================================================\n"))
	print(add1(model, ~.^2,test="Chisq"))
	cat(noquote("\n"))

	# stepwise selection backward
	cat(noquote("========================================\n"))
	cat(noquote("== Model Selection: Backward Stepwise ==\n"))
	cat(noquote("========================================\n"))
	print(summary(step(model, ~., direction = "backward")))
	cat(noquote("\n"))

	# forward stepwise selection
	cat(noquote("=======================================\n"))
	cat(noquote("== Model Selection: Forward Stepwise ==\n"))
	cat(noquote("=======================================\n"))
	print(summary(step(model, ~., direction = "forward")))
	cat(noquote("\n"))

	# stepwise selection, both forward and backward
	cat(noquote("=========================================================\n"))
	cat(noquote("== Model Selection: Both Forward and Backward Stepwise ==\n"))
	cat(noquote("=========================================================\n"))
	print(summary(step(model, ~., direction = "both")))
	cat(noquote("\n"))
}