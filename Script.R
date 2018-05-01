library(ggplot2)

#faithful is a built-in data set of Old Faithful geyser 
#Eruptions - how long an eruption lasts in minutes 
#Waiting - how long inbetween eruptions in minutes

#Initial plot of the variable to determine if a linear relationship exists
qplot(data$waiting, data$eruption, data, xlab="Waiting Time (min)", ylab="Eruption Duration (min)", main="Duration of Eruption vs. Time in Between Eruptions")

#Create a train and test set
set.seed(1)
row.number <- sample(1:nrow(faithful), 0.8*nrow(faithful))
train <- faithful[row.number,]
test <- faithful[-row.number,]

#Create linear model
eruption.lm <-  lm(formula = eruptions ~ waiting, data=train)
coeffs <- coef(eruption.lm)

#Predict the eruption time given a waiting time (85 min)
duration <- function(waitingTime) {
  coeffs[1]+coeffs[2]*waitingTime
}
duration(85)

#Or...
predictorData <- data.frame(waiting=85)
predict(eruption.lm, predictorData)

#Calaculate statistics on the model
summary(eruption.lm)

#Calculate the 95% confidence interval of a waiting time of 85 min
predict(eruption.lm, predictorData, interval="confidence")

#Calculate the 95% prediction interval of a waiting time of 85 min
predict(eruption.lm, predictorData, interval="predict")

#Plot the residuals
eruption.res <- resid(eruption.lm)
plot(train$waiting, eruption.res, ylab="Residuals", xlab="Waiting Time", main="Old Faithful Eruptions") 
abline(0,0)

#Plot the standardized residual
eruption.stdres <- rstandard(eruption.lm)
plot(train$waiting, eruption.stdres, ylab="Standardized Residuals", xlab="Waiting Time", main="Old Faithful Eruptions")
abline(0,0)

#Plot the normalized probablity
qqnorm(eruption.stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Old Faithful Eruptions") 
qqline(eruption.stdres)







