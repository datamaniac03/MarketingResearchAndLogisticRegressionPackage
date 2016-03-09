#' Interleaved Relever
#'
#' Interleaved, relevels factors so they can be plotted from highest to lowest frequency
#' x is a data frame
#' Response is a factor with two levels
#' colName is the column to be compared to the response
#' @param x, Response, colName
#' @keywords pretty plots factors interleaved
#' @export
#' @examples
#' InterleavedRelever()

InterleavedRelever <- function(x, Response, colName) {
	name1 <- names(sort(summary(x), decreasing=TRUE))
	L <- levels(x)
	N <- length(L)
	relevels <- rep(0,N) 
	for(i in 1:N) {
		for(j in 1:N) {
			if(name1[i] == L[j]){
				relevels[i] <- j
			}
		}
	Levels <- factor(x,levels(x)[relevels])
	}
	plot1 <- ggplot(as.data.frame(Levels), aes(x = Levels, fill = Response)) + 
	geom_bar(colour = "black", stat = "count", size=1, position="dodge") +
	aes(colour = x) + 
	ggtitle(paste("Barplot for", colName)) + 
	labs(x = "Levels", y = "Count")
	return(plot1)
}
