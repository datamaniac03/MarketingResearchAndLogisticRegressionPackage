#' Barplots for a dataframe (> 1 column)
#'
#' Returns a barplot, x is a data frame
#' @param x A data frame
#' @keywords ordered factor barplots
#' @export
#' @examples
#' GetNomBarPlots()

GetNomBarPlots <- function(x) {
	colName <- colnames(x)
	N <- length(colName)
	Plots <- list()
	for(i in 1:N) {
		Plots[[i]] <- Relever(x[,i], colName[i])
	}
	return(Plots)
}