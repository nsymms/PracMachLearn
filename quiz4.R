
# Question 1 ---------------------------------------------------------

library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)

fit1 <- train(y ~ ., data=vowel.train, method='rf')
fit2 <- train(y ~ ., data=vowel.train, method='gbm')

p1 <- predict(fit1, newdata=vowel.test)
p2 <- predict(fit2, newdata=vowel.test)

confusionMatrix(p1, vowel.test$y)$overall[1]
confusionMatrix(p2, vowel.test$y)$overall[1]
confusionMatrix(p1[p1==p2], vowel.test$y[p1==p2])$overall[1]




# Question 2 ----------------------------------------------------------

require(caret)
require(gbm)
require(MAAS)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

fit1 <- train(diagnosis ~ ., data=training, method='rf',
              trControl = trainControl(method='cv'), number=3)
fit2 <- train(diagnosis ~ ., data=training, method='gbm')
fit3 <- train(diagnosis ~ ., data=training, method='lda')

p1 <- predict(fit1, newdata=testing)
p2 <- predict(fit2, newdata=testing)
p3 <- predict(fit3, newdata=testing)

pDF <- data.frame(p1, p2, p3, diagnosis=testing$diagnosis)
fitC <- train(diagnosis ~ ., data=pDF, method='rf',
              trControl = trainControl(method='cv'), number=3)
pC <- predict(fitC, newdata=testing)

confusionMatrix(p1, testing$diagnosis)$overall
confusionMatrix(p2, testing$diagnosis)$overall
confusionMatrix(p3, testing$diagnosis)$overall
confusionMatrix(pC, testing$diagnosis)$overall


# Question 3 ------------------------------------------------------

library(AppliedPredictiveModeling)
data(concrete)
set.seed(233)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

fit1 <- train(CompressiveStrength ~ ., data=training,
              method='lasso', trace=T)
# OR
o1<-lars(as.matrix(training[,-9]),training[,9], type ="lasso",trace=TRUE)
# OR
o1 <- enet(as.matrix(training[,-9]), training[,9], lambda=0)
plot(o1, xvar='penalty') # Cement is last


# Question 4 -------------------------------------------------------

library(lubridate)  # For year() function below
library(forecast)
dat = read.csv("gaData.csv")
training <- dat[year(dat$date) < 2012,]
testing <- dat[(year(dat$date)) > 2011,]
tstrain <- ts(training$visitsTumblr)
tstest <- ts(testing$visitsTumblr)

fit1 <- bats(tstrain)
fc1 <- forecast(fit1, h=nrow(testing), level=95)

summary(testing$visitsTumblr >= fc1$lower &
            testing$visitsTumblr <= fc1$upper)

226 / (9+226)

# Question 5 -----------------------------------------------------------

set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
fit1 <- svm(CompressiveStrength ~ ., data=training)

pd1 <- predict(fit1, newdata=testing)

sqrt(mean((testing$CompressiveStrength - pd1)^2))

