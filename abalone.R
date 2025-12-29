library(e1071)
library(caret)
library(corrplot)
library(ggplot2)
library(polycor)
library(Metrics)
library(tidyverse)

abaloneData <- read.csv("C:/Users/moina/MSDS_6306_Doing-Data-Science/Unit 14 and 15 Case Study 2/train2.csv")

# Polychorric correlation matrix. Can handle non-numeric data
selectDf = abaloneData[, -c(1)]
cmat = hetcor(selectDf)
corrplot(cmat$correlations, method = "circle", order = "hclust")

# From correlation matrix we can see the strongest correlation between Shell.Weight and Age
set.seed(5)
trainIndices = sample(1:dim(abaloneData)[1], round(0.7 * dim(abaloneData)[1]))
train = abaloneData[trainIndices,]
test = abaloneData[-trainIndices,]

fit_1 <- lm(Age ~ Shell.Weight + Height + Diameter + Length, data = train)
#fit_1 <- lm(Age ~ Shell.Weight + Height + Diameter + Shucked.Weight, data = train)
#fit_1 <- lm(Age ~ Shell.Weight + Viscera.Weight + Shucked.Weight, data = train)
summary(fit_1)

fit_2 <- lm(Age ~ Shell.Weight, data = train)
summary(fit_2)

fit_3 <- lm(Age ~ Shell.Weight + Viscera.Weight, data = train)
summary(fit_3)

# Final model
fit_4 <- lm(Age ~ Shell.Weight * Height * Diameter * Shucked.Weight, data = train)
summary(fit_4)

pred1 <- predict(fit_1, test[,c(3,4,5,9)])
pred2 <- predict(fit_2, data.frame(Shell.Weight = test$Shell.Weight))
pred3 <- predict(fit_3, test[,c(8,9)])
pred4 <- predict(fit_4, test[,c(3,5,7,9)])

mae <- mae(test$Age, pred1)
print(mae)
mae <- mae(test$Age, pred2)
print(mae)
mae <- mae(test$Age, pred3)
print(mae)
mae <- mae(test$Age, pred4)
print(mae)

# Mae of pred4 is lowest
ggplot(data=train, aes(fit_4$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

# Histogram of model residuals is close to normal curve around 0. So model 3 fits the data well
# There is a very slight skewness to the right though

# Now train using 10-fold cross-validation on the train data
train_control <- trainControl(method = "repeatedcv", number = 10,
                              repeats = 3)
model <- train(Age ~ Shell.Weight * Height * Diameter * Shucked.Weight,
               data = train, method = "lm", trControl = train_control)
predictions <- predict(model, newdata = test[,c(3,5,7,9)])
print(mae(test$Age, predictions))




ggplot(data=train, aes(x = Shell.Weight, y = Age)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

