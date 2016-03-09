################################################################################
# This code uses the LogisticRegression package to analyze some marketing data #
################################################################################

# Dependencies
require(LogisticRegression)
require(pROC) 			# Gets the auc
require(ROCR)				# Gets appx auc
require(ggplot2) 			# Gets plots
require(LICORS) 			# Gets kmeans++
require(grid)				# For the multiple ggplot function
require(dplyr) 			# For munging data
require(scales) 			# For plots
require(reshape) 			# For munging data
require(aod) 				# For wald tests

#################
# Load the data #
#################
load("ExampleData.RData")

#########################
# Separate the response #
#########################
Response 	<- Data$y
Data$y 	<- NULL

######################
# Print data summary #
######################
summary(Data)

#####################################################################
# Remove variables: day, pdays, month, duration, campaign, previous #
#####################################################################
Data$day 	 <- NULL
Data$pdays 	 <- NULL
Data$month 	 <- NULL
Data$duration	 <- NULL
Data$campaign	 <- NULL
Data$previous	 <- NULL

###############################################
# Rename the response with Yes = 1 and No = 0 #
###############################################
w 		<- which(Response == "no")
Response[w] 	<- "0"
w 		<- which(Response == "ye")
Response[w] 	<- 1

##################################################
# Use k-means clustering to segment the Balances #
##################################################
#######################################
# Remove the rows with missing values #
#######################################
w <- which(is.na(Data$balance)==TRUE)
BalanceData 		<- Data[-w,]
BalanceResponse 	<- Response[-w]
BalanceData$age 	<- NULL
Data1 			<- Data[w,]
Response1 		<- Response[w]

#####################################################
# Choose the number of levels to split balance into #
#####################################################
# In this example the maximum k we'll look at is 4
# Follow the instructions in the console so that you can view each clustering as it happens
# If you are running a windows machine, after the clustering, 
# you can use the arrow keys to scroll through the plots
BalanceOut <- findK(BalanceData, BalanceResponse, 5, 4, "Balance")


###############################################################
# Recombine with missing and assign missing as a factor level #
###############################################################
colnames(BalanceOut)[9] 	<- "NewBalance"
NewBalance 			<- rep("cl 0", length(Data1$balance))
Data1$age 			<- NULL
Data1$balance 		<- NULL
Data1 				<- cbind(Data1, NewBalance)
Balance 			<- rbind(Data1, BalanceOut)

##############################################
# Use k-means clustering to segment the Ages #
##############################################
############################################
# For clustering remove the missing values #
############################################
w <- which(is.na(Data$age)==TRUE)
AgeData 		<- Data[-w,]
AgeResponse 		<- Response[-w]
AgeData$balance 	<- NULL
Data2 			<- Data[w,]
rm(w)

#################################################
# Choose the number of levels to split age into #
#################################################
# The maximum k here is 6
# Follow the instructions in the console so that you can view each clustering as it happens
# If you are running a windows machine, after the clustering, 
# you can use the arrow keys to scroll through the plots
AgeOut <- findK(AgeData, AgeResponse, 1, 6, "Age")

###############################################################
# Recombine with missing and assign missing as a factor level #
###############################################################
colnames(AgeOut)[9] <- "NewAge"
NewAge 		<- rep("cl 0", length(Data2$age))
Data2$balance		<- NULL
Data2$age 		<- NULL
Data2 			<- cbind(Data2, NewAge)
Age 			<- rbind(Data2, AgeOut)
NewAge 		<- Age$NewAge

#######################
# Logistic Regression #
#######################
	##################################
	# Combine old data with new data #
	##################################
	NewData 	<- cbind(Balance, NewAge)
	Response 	<- factor(c(Response1, BalanceResponse))
	# rm(Balance, NewAge, Response1)

	# Convert the character vectors to factors
	NewData <- Char2Factors(NewData)

	##############################################################
	# Split the data into train and test for logistic regression #
	##############################################################
	# Split the data into training and test
	NewDataSplit	<- SplitData(NewData, Response)
	xtrain 	<- NewDataSplit[[1]]
	ytrain 	<- NewDataSplit[[2]]
	xtest 		<- NewDataSplit[[3]]
	ytest 		<- NewDataSplit[[4]]
	rm(NewData, NewDataSplit)

	###########################
	# Fit Logistic Regression #
	###########################
	# This function prints lots of diagnostics and model information for the logistic
	# regression, it takes a little while, but is useful for exploratory purposes
	# It uses 3 error functions: auc, log-loss binary, and 0-1 loss and also prints a confusion matrix
	# It also does model selection with and without second order interactions and returns four plots
	# Returns an object of type model for further selection below

	LogisticGLMAge <- LogisticGLM(xtrain, ytrain, xtest, ytest)

	###################
	# Model Selection #
	###################
	# Prepare submodels
	LogisticGLMAge0 <- update(LogisticGLMAge, ~ . - loan - contact - NewAge)
	LogisticGLMAge1 <- update(LogisticGLMAge, ~ . - loan - contact)
	
	# Check out the submodels
	anova(LogisticGLMAge0, LogisticGLMAge1)
	anova(LogisticGLMAge0, LogisticGLMAge)
	anova(LogisticGLMAge1, LogisticGLMAge)

	# Test coefficients
	# SubsetVs <- cbind(0,0,0,0,1,1,0,0,0,0,0,...,)
	# wald.test(b = coef(LogisticGLMAge), Sigma = vcov(LogisticGLMAge), L = SubsetVs)

	# Some Automatic Selection
	drop1(LogisticGLMAge, test="Chisq")
	add1(LogisticGLMAge, ~.^2,test="Chisq")
	search <- step(LogisticGLMAge, ~., direction = "both")
