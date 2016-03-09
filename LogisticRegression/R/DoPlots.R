#' Plot information about predictions on test set
#' 
#' Plots prediction information
#' predictions are from a model predict object
#' performance1 is the output from the LossInfo function
#' ytest is a factor with two levels
#' @param predictions, performance1, ytest
#' @keywords plots
#' @export
#' @examples
#' DoPlots()

DoPlots <- function(predictions, performance1, ytest) {

	# Plot histogram of predicted probabilities using ggplot2
	plot1 <- qplot(x = predictions, geom = "histogram", binwidth = .01, xlab = "Predicted Probability", ylab = "Count", 
			main = "Predicted Probabilities for Logistic Regression", fill = I("dark blue"), col = I("orange")
			)

	# plot ROC
	roc1 <- data.frame(fpr=unlist(performance1@x.values), tpr=unlist(performance1@y.values))
  	plot4 <- ggplot(data= as.data.frame(roc1), aes(x=fpr, y=tpr)) + 
  		geom_line(colour = "red") +
		geom_abline(intercept = 0, slope = 1) +
		ggtitle("ROC Curve") +
		xlab("False Positive Rate") +
		ylab("True Positive Rate") +
  		scale_x_continuous(labels=percent, lim=c(0,1)) +
  		scale_y_continuous(labels=percent, lim=c(0,1))
	
	# Plot calibration
	plot2 <- data.frame(Predicted = predictions, Actual = as.vector(ytest)) %>%
		group_by(Predicted = round(Predicted * 10) / 10) %>%
		summarize(Count=n(), Actual = mean(Actual == "1")) %>%	
		ggplot(data = ., aes(x = Predicted, y = Actual, size = Count)) +
		geom_point(colour = "dark blue") +
		ggtitle("Calibration Plot for Logistic Regression") +
		scale_x_continuous(labels = percent, lim = c(0,1)) +
		scale_y_continuous(labels = percent, lim = c(0,1))

	# plot distribution of the predicted labels split by the actual label
	plot3 <- data.frame(predicted = predictions, actual = as.vector(ytest)) %>%
		ggplot(data = ., aes(x = predictions)) +
		geom_density(aes(fill = as.vector(ytest)), alpha = 0.5) +
		ggtitle("Distribution of Predicted Labels Split by Actual for Logistic Regression") +
		xlab("Predicted Probability") +
		ylab("Density") +
		scale_fill_discrete(name = "Actual label") +
		theme(legend.position = c(0.8, 0.8))
	
	# Use the multiplot function to place all 4 plots on the same page
	multiplot(plotlist = list(plot1, plot2, plot3, plot4), cols = 2)
}