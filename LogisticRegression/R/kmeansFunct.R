#' Run k-means++ and check logistic regression model
#'
#' Calculates k-means++ clustering, splits data, fits logistic regression
#' xtrain and xtest are data frames
#' ytrain and ytest are response vectors with two levels
#' col is the number of the column to be used
#' k is the number of clusters, a positive integer
#' colName is the name of the column to be used
#' @param xtrain, ytrain, xtest, ytest,col,k, colName
#' @keywords K-means segmentation
#' @export
#' @examples
#' kmeansFunct()

kmeansFunct <- function(xtrain, ytrain, xtest, ytest,col,k, colName) {

	X <- rbind(xtrain,xtest)

	# Fit k-means++ to the specified variable
	b <- X[,col]
	kms <- kmeanspp(b,k)
	Cluster <- factor(kms$cluster)

	# Transforms the original variable to a categorical variable 
	xtrain[,col] <- Cluster[1:length(xtrain[,1])]
	xtest[,col] <- Cluster[(length(xtrain[,1])+1):length(X[,1])]		
	
	# Prints statistcs for each individual cluster
	ClusterStats <- GetClustStats(Cluster, b, k)

	# Fit the glm logistic model
	m1 <- glm(ytrain ~ ., data = as.data.frame(xtrain), family="binomial")
		prb <- predict(m1, as.data.frame(xtest), type="response")
		AU1 <- auc(ytest, prb)
		ZO1 <- ZeroOneLoss(ytest, prb)
		LL1 <- LogLossBinary(as.numeric(ytest)-1, prb)
		
	# Prints prediction vs. actual details and cluster information
	ReturnTables(prb, ytest, ZO1, LL1, AU1, ClusterStats)

	# Plots the clusters
	PlotClusters(b, Cluster, colName)

	cat(noquote("[close the graphics device or press esc in the console to stop]\n"))
	# Pauses the function until R graphic device is clicked and a key is pressed 
	keyPressed = readkeygraph("[press any key to continue]")
	
	# To automate the releveling
	return(ClusterStats[,3:4])
}


