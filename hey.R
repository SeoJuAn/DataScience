library(tidyverse)
library(rpart)
library(e1071)
library(caret)
library(nnet)

set.seed(100)
getModelInfo()
modelLookup("nnet")
getwd()

fishing<- read.csv("Fishing.csv")
#print(fishing)

str(fishing)
data <- fishing

# select variables
mode<- factor(data$mode, levels = c("beach", "boat", "charter", "pier" ))
price <- as.numeric(data$price) 
catch<- as.numeric(data$catch)
income<- as.numeric(data$income)
ds<- data.frame(mode, price, catch, income)
str(ds)

# data preprocessing using caret::preProcess
# center and scale 
preProcValues = preProcess(ds) 
#The function preProcess estimates the required parameters for each operation and predict.preProcess is used to apply them to specific data sets. This function can also be interfaces when calling the train function.
ds <- predict(preProcValues, ds)

print(summary(ds))

# split the data into tr and ts sets 
set.seed(4) # random seed
indexes = createDataPartition(ds$mode, p = .6, list = F)
train = ds[indexes, ]
test = ds[-indexes, ]
train
test

# base model rpart

fit = rpart(mode~., 
            data = train
)

printcp(fit)
library(rattle)
fancyRpartPlot(fit)

pred = predict(fit, test, type = "class" )
print(data.frame(test, pred))
confusionMatrix(pred, test$mode)

modelLookup("nnet")

# optimization
trControl=trainControl(method='repeatedcv', number = 10, repeats = 2)
model = train(mode ~.,
              data = train,
              method = 'nnet',
              maxit = 50,
              metric = 'Accuracy',
              # preProcess = c('center', 'scale'), # data normalization
              # We dont need to this, because the data is already scaled
              
              trControl = trControl,
              #tuneGrid=grid
              tuneLength = 3
)