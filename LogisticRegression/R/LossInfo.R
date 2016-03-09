#' Calculate and print loss
#'
#' Prints loss function information
# 
#' @param model, predictions, xtest, ytest
#' @keywords loss
#' @export
#' @examples
#' LossInfo()

LossInfo <- function(model, predictions, xtest, ytest) {

	# Get the ROC curve
	pred <- prediction(predictions, ytest)
	perf <- performance(pred, measure='tpr', x.measure='fpr')
	perf2 <- performance(pred, 'auc')
	
	# Approximate AUC
	# sample pairs of randomly selected positive and negative examples 
	# compute the fraction of time the positive example scored higher than the negative one
	predicted <- predictions
	actual <- ytest == "1"
	ndx_pos <- sample(which(actual == 1), size = 5000, replace = TRUE)
	ndx_neg <- sample(which(actual == 0), size = 5000, replace = TRUE)
	APX_AUC <- mean(predicted[ndx_pos] > predicted[ndx_neg])

	# Compute loss
	AUC_LR <- perf2@y.values[[1]]					# compute auc
	LL_LR <- LogLossBinary(as.numeric(ytest) - 1, predicted)	# compute the log loss
	ZO_LR <- ZeroOneLoss(ytest, predicted)				# compute the 0-1 loss
	ZOR_LR <- ZeroOneLossRound(ytest, predicted)			# compute the rounded 0-1 loss
	Error_LR <- rbind(ZO_LR, ZOR_LR, AUC_LR, APX_AUC, LL_LR)	# combine computations into a single table

	cat(noquote("======================\n"))
	cat(noquote("== Confusion Matrix ==\n"))
	cat(noquote("======================\n"))
	# Look at confusion matrix
	cmat <- table(predict(model, xtest) > 0, ytest)
	rownames(cmat) <- c("0","1")
	print(cmat)
	cat(noquote("\n"))

	cat(noquote("================\n"))
	cat(noquote("== Test Error ==\n"))
	cat(noquote("================\n"))
	colnames(Error_LR) <- c("Error")
	rownames(Error_LR) <- c("0-1", "0-1 Rounded", "AUC", "Appx. AUC", "Log-Loss")
	print(Error_LR)	
	cat(noquote("\n"))

	# Returned for plotting
	return(perf)
}