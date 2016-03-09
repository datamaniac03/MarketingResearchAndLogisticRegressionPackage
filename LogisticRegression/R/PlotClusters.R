#' Plot Clusters
#'
#' Plots the k-means++ clustering in two ways
#' x1 is a vector, Cluster is the cluster output from kmeans++
#' colName is the name of the column that has been clustered
#' @param x1, Cluster, colName
#' @keywords K-means segmentation plots
#' @export
#' @examples
#' PlotClusters()

PlotClusters <- function(x1, Cluster, colName) {

	# Plot the points
	p1 <- ggplot(as.data.frame(x1), aes(x = seq(length(x1)), y = x1)) + 
	geom_point() + 
	aes(colour = Cluster) + 
	ggtitle(paste("K-Means Clustering by", colName)) + 
	labs(x= "Index", y = colName) + 
	scale_colour_discrete(name = "Cluster")

	# Plot count barplots
	p2 <- ggplot(as.data.frame(x1), aes(x = Cluster, fill = Cluster)) + 
	geom_bar(colour = "black", stat = "count", size=1) +
	aes(colour = Cluster) + 
	ggtitle(paste("K-Means Clustering by", colName)) + 
	labs(x = "Cluster", y = "Number of Cluster Members")

	# Plot both on one page
	p3 <- multiplot(plotlist = list(p1, p2), col=2)
}

