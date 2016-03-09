#' User Set Cluster Levels
#'
#' Allows user to choose a segmentation found with k-means
#' X is a data frame, ClusterLevels is a list of clusters, 
#' col is the column number of the column being clustered
#' @param X, ClusterLevels, col
#' @keywords K-means segmentation
#' @export
#' @examples
#' SetClusterLevels()

SetClusterLevels <- function(X, ClusterLevels, col) {

	k <- readline("[Enter the number of clusters]\n")
 	k <- as.numeric(k)
	Min1 <- ClusterLevels[[k]][,1]
	Max1 <- ClusterLevels[[k]][,2]
	OldVar <- X[,col]
	NewVar <- rep("x", length(OldVar))

	for(i in 1:k) {
		mini <- Min1[i]
		maxi <- Max1[i]
		w <- which(OldVar >= mini & OldVar <= maxi)
		NewVar[w] <- paste("cl", i) 
	}

	newX <- cbind(X, NewVar)	
	newX[,col] <- NULL
	return(newX)
}
