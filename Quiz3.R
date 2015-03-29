

# Question 2 ------------------------------------------------------
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

train.rows <- segmentationOriginal$Case == 'Train'

trainD <- segmentationOriginal[train.rows,]
testD <- segmentationOriginal[!train.rows,]
set.seed(125)

fit<-train(Class ~ ., trainD[,3:119], method='rpart')

# Question 3 ------------------------------------------------------

library(caret)
library(rpart)
library(tree)

load('olive.rda')
olive<-olive[,-1]

fit <- train(Area ~ ., olive, method='rpart')

fit <- tree(Area ~ ., olive)

# Question 4 ------------------------------------------------------

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

fit <- train(chd ~ age+alcohol+obesity+tobacco+typea+ldl,
             data=trainSA, method='glm', family='binomial')

missClass = function(values,prediction) {
    sum(((prediction > 0.5)*1) != values)/length(values)
}

pred <- predict(fit, newdata=testSA)
missClass(testSA$chd, pred)

pred.train <- predict(fit, newdata=trainSA)
missClass(trainSA$chd, pred.train)


# Question 5 ----------------------------------------------------------
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

fir <- randomForest(y ~ ., data=vowel.train)
fit <- train(y ~ ., data=vowel.train, method='rf')

varImp(fit)

