# Loading libraries
library(Metrics)
library(neuralnet)
library(readxl)
library(caret)
library(nnfor)
library(useful)

# Loading the dataset
loadDf <- read_excel("D:/IIT/Year 2 Sem 2/Data Mining & Machine Learning/CW/UoW_load.xlsx")

# Converting Date column to pure numeric values
colnames(loadDf)
loadDf$Dates <- as.numeric(loadDf$Dates)
colnames(loadDf)

# Min-Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindf <- as.data.frame(lapply(loadDf, normalize))
boxplot(loadDf)
boxplot(maxmindf)

# Changing the column names to strings for easy accessibility
colnames(maxmindf) <- c("Dates", "Nine", "Ten", "Eleven")

# Splitting dataset into training and testing data
trainSet.load <- maxmindf[1:430, ]
testSet.load <- maxmindf[431:500, ]

# AUTOREGRESSION MODEL 
# Training the network with 2, 3 as hidden layers
set.seed(1231)
nn.ar <- neuralnet(Eleven ~ Dates+Eleven, data=trainSet.load, hidden=c(2,3), linear.output=TRUE, threshold=0.01)
nn.ar$result.matrix
nn.ar$data
plot(nn.ar)

# Predicting using testSet.load
myPred.ar <- predict(nn.ar, testSet.load)

# Converting actual and predictinon to a dataframe  
results.ar <- data.frame(actual = testSet.load$Eleven, prediction = myPred.ar)

# Finding accuracy
predicted.ar=myPred.ar * abs(diff(range(testSet.load$Eleven))) + min(testSet.load$Eleven)
actual.ar=testSet.load$Eleven * abs(diff(range(testSet.load$Eleven))) + min(testSet.load$Eleven)
comparison=data.frame(predicted.ar,actual.ar)
deviation.ar=((actual.ar-predicted.ar)/actual.ar)

# Removing the infinite value
deviation.ar[46] <- NA
deviation.ar
d2.ar <- na.omit(deviation.ar)
d2.ar
comparison=data.frame(predicted.ar,actual.ar,deviation.ar)
accuracy.ar=1-abs(mean(d2.ar))
accuracy.ar

# Calculate Root Mean Square Error (RMSE)
rmse(actual.ar, predicted.ar)
# Calculating Mean Absolute Error
mae(actual.ar, predicted.ar)
# Calculating Mean Absolute Percentage Error
mape(predicted.ar, actual.ar)
#Plotting actual vs predicted graph
plot(testSet.load$Eleven, predicted.ar, col='blue', pch=16, ylab = "predicted", xlab = "actual", abline(a = 0, b = 1))

# NARX MODEL
nn.narx <- neuralnet(Eleven ~ Dates+Eleven+Ten+Nine, data=trainSet.load, hidden=c(1,2), linear.output=TRUE, threshold=0.01)
nn.narx$result.matrix
nn.narx$data
plot(nn.narx)

# Predicting using testSet.load
myPred.narx <- predict(nn.narx, testSet.load)

# Converting actual and predictinon to a dataframe  
results.narx <- data.frame(actual = testSet.load$Eleven, prediction = myPred.narx)

# Finding accuracy
predicted.narx=myPred.narx * abs(diff(range(testSet.load$Eleven))) + min(testSet.load$Eleven)
actual.narx=testSet.load$Eleven * abs(diff(range(testSet.load$Eleven))) + min(testSet.load$Eleven)
comparison=data.frame(predicted.narx,actual.narx)
deviation.narx = ((actual.narx-predicted.narx)/actual.narx)

# Removing the infinite value
deviation.narx[46] <- NA
deviation.narx
d2.narx <- na.omit(deviation.narx)
d2.narx
comparison=data.frame(predicted.narx,actual.narx,deviation.narx)
accuracy.narx=1-abs(mean(d2.narx))
accuracy.narx

# Calculate Root Mean Square Error (RMSE)
rmse(actual.narx, predicted.narx)
# Calculating Mean Absolute Error
mae(actual.narx, predicted.narx)
# Calculating Mean Absolute Percentage Error
mape(predicted.narx, actual.narx)
#Plotting actual vs predicted graph
plot(testSet.load$Eleven, predicted.narx, col='blue', pch=16, ylab = "predicted", xlab = "actual", abline(a = 0, b = 1))

