% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmeansFunct.R
\name{kmeansFunct}
\alias{kmeansFunct}
\title{Run k-means++ and check logistic regression model}
\usage{
kmeansFunct(xtrain, ytrain, xtest, ytest, col, k, colName)
}
\arguments{
\item{xtrain, }{ytrain, xtest, ytest,col,k, colName}
}
\description{
Calculates k-means++ clustering, splits data, fits logistic regression
xtrain and xtest are data frames
ytrain and ytest are response vectors with two levels
col is the number of the column to be used
k is the number of clusters, a positive integer
colName is the name of the column to be used
}
\examples{
kmeansFunct()
}
\keyword{K-means}
\keyword{segmentation}

