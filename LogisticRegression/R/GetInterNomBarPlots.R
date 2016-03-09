#' Interleaved Barplots for a dataframe (> 1 column)
#'
#' For a multiple class response ( > 1) where x is a data frame and 
#' response is a factor vector with two levels
#' @param response, x
#' @keywords ordered factor interleaved barplots
#' @export
#' @examples
#' GetInterNomBarPlots()

GetInterNomBarPlots <- function(data, Response) {
	colName <- colnames(data)
	N <- length(colName)
	Plots <- list()
	for(i in 1:N) {
		Plots[[i]] <- InterleavedRelever(data[,i], Response, colName[i])
	}
	return(Plots)
}