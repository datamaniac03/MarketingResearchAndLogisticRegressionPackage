#' Print Segmentation Diagnostics
#'
#' Tables for segmentation diagnostics
#' prb is a probability
#' ytest is a factor with two levels
#' Z01, LL1, AU1 are outputs from LossInfo
#' @param prb, ytest, ZO1, LL1, AU1, Ranges
#' @keywords K-means segmentation plots
#' @export
#' @examples
#' ReturnTables()

ReturnTables <- function(prb, ytest, ZO1, LL1, AU1, Ranges) {

	cat(noquote("== Confusion Matrix ==\n"))
	print(table(round(prb), ytest))
	cat(noquote("\n"))
		
	cat(noquote("== Summary of Predicted Values ==\n"))
	print(summary(prb))
	cat(noquote("\n"))
		
	cat(noquote("== Test Error ==\n"))
	Error <- rbind(ZO1,LL1,AU1)
	colnames(Error) <- c("Error")
	rownames(Error) <- c("0-1", "LogLoss", "AUC")
	print(Error)
	cat(noquote("\n"))
	
	cat(noquote("== Statistics for cluster k ==\n"))
	print(Ranges)
	cat(noquote("\n"))
}
