#' Relever
#'
#' Relevels factors so they can be plotted from highest to lowest frequency
#' x is a factor, colName is its name
#' @param x, colName
#' @keywords pretty plots factors
#' @export
#' @examples
#' Relever()

# Sorts the levels of a factor to make the barplots more aesthetically appealling
Relever <- function(x, colName) {
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
	plot1 <- ggplot(as.data.frame(Levels), aes(x = Levels, fill = Levels)) + 
	geom_bar(colour = "black", stat = "count", size=1) +
	aes(colour = x) + 
	ggtitle(paste("Barplot for", colName)) + 
	labs(x = "Levels", y = "Count")
	return(plot1)
}
