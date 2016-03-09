#' Find K Segments
#'
#' Segments numerical variables into categories using k-means
#' X is a data frame, Y is a response that is a factor with two levels
#' @param X, Y, col, maxK, colName
#' @keywords K-means segmentation
#' @export
#' @examples
#' findK()

findK <- function(X,Y,col,maxK,colName) {

	# Starts the recording of plots so they can be easily accessed after the program runs, only works with windows
	windows(record=TRUE)

	# Arranges screens so plots can be seen easily
	arrangeWindows("v") 

	# Skips several lines before printing output to make console output easier to read
	cls()

	# Some console print and aesthetics	
	cat(noquote("==============================================\n"))
	cat(noquote("Printing the output of each test k for K-means\n"))
	cat(noquote("==============================================\n"))

	# Split the data into train and test
	splitData <- SplitData(X, Y)
	xtrain <- splitData[[1]]
	ytrain <- splitData[[2]]
	xtest  <- splitData[[3]]
	ytest  <- splitData[[4]]

	# Define a variable to hold the minimum and maximum of the clustering levels
	ClusterLevels <- list()
	ClusterLevels[[1]] <- "cluster records start at index 2"

	# Begin the loop to reun the individual k's up to the specified maximum k
	for(k in 2:maxK) {
	
	# Console output aesthetics, prints the current k
	cat(noquote("===============================================\n"))
	cat(noquote("\n"))
	cat("Summary for k-means with k = ", iter <- k)
	cat(noquote("\n"))
	cat(noquote("\n"))

	# Runs kmeans++, generates plots, examines prediction with glm and prints confusion matrices 
	# and loss function information relating to test data
	ClusterLevels[[k]] <- kmeansFunct(xtrain, ytrain, xtest, ytest,col,k, colName)
	}

	# More console output aesthetics
	cat(noquote("===========================================================\n"))
	cat(noquote("=== End ===\n"))
	cat(noquote("===========================================================\n"))

	return(SetClusterLevels(X, ClusterLevels, col))
}