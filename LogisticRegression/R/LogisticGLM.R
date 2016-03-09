#' Logistic Regression
#'
#' This function calculates and plots Logistic Regression via glm()
#' xtrain and xtest are data frames, 
#' @param xtrain, ytrain, xtest, ytest
#' @keywords Logistic Regression
#' @export
#' @examples
#' LogisticGLM()

LogisticGLM <- function(xtrain, ytrain, xtest, ytest) {

	# Clear console to make reading easier
	cls()

	# Run the model
	model <- glm(ytrain ~ ., data = xtrain, family = "binomial")

	# Get the predictions
	predictions <- predict(model, xtest, type="response")

	# Print model summary and tests
	PrintModelInfo(model, xtest, ytest)

	# Compute the confusion matrix and loss
	performance <- LossInfo(model, predictions, xtest, ytest)

	# Look at variable selection
	VarSelection(model)

	# Plots
	DoPlots(predictions, performance, ytest)

	# Return the model for possible subset tests
	return(model)
}