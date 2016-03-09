#' Get the cluster statistics
#'
#' Table of within cluster statistics
#' Cluster is a vector of integers indicating clusters
#' x is a vector and k is a non-negative integer
#' @param Clusters, x, k
#' @keywords K-means segmentation plots
#' @export
#' @examples
#' GetClustStats()

GetClustStats <- function(Clusters, x, k) {

	# define the matrix to contain the results
	lout <- matrix(0, k, 5)

	# Calculate the statistics for each cluster
	for(i in 1:k) {
		w <- which(Clusters == i)
		x1 <- x[w]
		lout[i,] <- c(mean(x1), median(x1), range(x1), length(x1)) 
	}

	# Writing the output table
	rownames(lout) <- paste("cluster", seq(1:k), spe="")
	colnames(lout) <- c("Centroid", " Median", "Min.", "Max.", "# of Obs.")
	return(lout)
}

