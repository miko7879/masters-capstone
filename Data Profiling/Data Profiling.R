#Load required libraries
library(pastecs)
library(corrplot)
library(timeSeries)

#Set the working directory
setwd("C:\\Users\\micha\\Documents\\Masters Capstone\\exact\\datasets\\2018_coal")

#Read in all 12 files and stack them
ds <- read.csv("burner_0.csv")
for (file_no in 1:11) {
  ds <- rbind(ds, read.csv(print(paste("burner_", file_no, ".csv", sep = ""))))
}

#Gram the summary statistics of each column
summary_stats <- stat.desc(ds)

#Generate plots of each column
par(mfrow = c(4, 3))
for (col in 1:12) {
  plot(ds[, col], main = paste("Plot of", colnames(ds)[col], sep = " "), ylab = colnames(ds)[col], xlab = "Minute", pch=16, cex=0.4)
}
par(mfrow = c(1, 1))


#Generate a histogram of each column
par(mfrow = c(4, 3))
for (col in 1:12) {
  hist(ds[, col], main = colnames(ds)[col], xlab = "Value")
}
par(mfrow = c(1, 1))

#Plot the correlations
corrs <- cor(ds)
corrplot(corrs, method = "number", tl.col = "black")

#TS Analysis of Response
ts <- ts(ds[, 12])
par(mfrow = c(2, 1))
acf(ts, main = "ACF of Response")
pacf(ts, main = "PACF of Response")
