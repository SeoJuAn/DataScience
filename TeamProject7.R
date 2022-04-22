library(tidyverse)
library(rpart)
library(e1071)
library(caret)
library(nnet)
library(Amelia)
library(rpart.plot)
library(kernlab) # svmLinear2
library(randomForest)
library(rpart.plot)

df<-read.csv('hcvdat0.csv'
             , stringsAsFactors = F
             , na.strings = c("","NA", "Unknown", "NULL"))
df$Category <- if_else(df$Category == '0=Blood Donor', 0,1)
df$Sex <- if_else(df$Sex == 'm',0,1)
df$Category <- as.factor(df$Category)

print(colSums(is.na(df)))
missmap(df, col=c("yellow", "black"), legend = FALSE)

#df_nomiss <- df %>% filter(!is.na(df))
#결측치 제거
df$ALB[is.na(df$ALB)] <- mean(df$ALB,na.rm = T)
df$ALP[is.na(df$ALP)] <- mean(df$ALP,na.rm = T)
df$ALT[is.na(df$ALT)] <- mean(df$ALT,na.rm = T)
df$CHOL[is.na(df$CHOL)] <- mean(df$CHOL,na.rm = T)
df$PROT[is.na(df$PROT)] <- mean(df$PROT,na.rm = T)

#결측치 재확인
#print(is.na(df))
print('--------------결측치 제거 후---------------')
print(colSums(is.na(df)))
missmap(df, col=c("yellow", "black"), legend = FALSE)


#nnet 모델 생성
set.seed(100)
getModelInfo()
modelLookup("nnet")
getwd()

#df <- data.frame(df)


# data preprocessing using caret::preProcess
# center and scale 
preProcValues = preProcess(df,method = c("center", "scale")) 
#The function preProcess estimates the required parameters for each operation and predict.preProcess is used to apply them to specific data sets. This function can also be interfaces when calling the train function.
df <- predict(preProcValues, df)

#print(summary(df))


set.seed(4) # random seed
indexes = createDataPartition(df$Category, p = .6, list = F)
train = df[indexes, ]
test = df[-indexes, ]

# optimize model - rough
modelLookup("nnet")

# optimization
trControl=trainControl(method='repeatedcv', number = 10, repeats = 2)
model = train(Category ~.,
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

#install.packages("devtools")
library(devtools)
library(neuralnet)
source('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


# show model 
print(model)
plot.nnet(model)
net.infert <- neuralnet(Category~Age+Sex+ALB+ALP+ALT+AST+BIL+CHE+CHOL+CREA+GGT+PROT, data = df, hidden = 1,
                        err.fct="ce", linear.output = FALSE, likelihood = TRUE)
plot(net.infert)


# show final model
print(model$finalModel)
X <- varImp(model$finalModel)
#plot(X)
# check the convergence of the final model 
model$finalModel$convergence 
# 1 if maxit reached, 0 otherwise

pred <- predict(model$finalModel, newdata = test, type = "class")
pred <- as.factor(pred)
#pred <- factor(pred, levels = c("beach", "boat", "charter", "pier" ))
#print(data.frame(test$Category, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
print(confusionMatrix(pred, test$Category))


#학습곡선
rpart_data <-
  learning_curve_dat(dat = df,
                     outcome = "Category",
                     test_prop = 1/4,
                     ## `train` arguments:
                     method = "nnet",
                     metric = "Accuracy")

p1 <- ggplot(rpart_data, aes(x = Training_Size, y = Accuracy, color = Data)) +
  geom_smooth(method = loess, span = .8) +
  theme_bw()
plot(p1)


#svm 모델 생성

names(getModelInfo("svm"))
modelLookup("svmLinear2")

# optimization
trControl <- trainControl(method='repeatedcv',number = 10, repeats = 2)
model1 <- train(Category ~.,
                data = train,
                method = 'svmLinear2',
                metric = 'Accuracy',
                tuneLength = 10,
                trControl = trControl
)

# show model 
model1

# plot(model1)
# show final model
model1$finalModel

# Model Evaluation
# Predict testing set
pred <- predict(model1, test) 
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$Category)
acc1 <- confusionMatrix(pred, test$Category)$overall['Accuracy']
print(acc1)



modelLookup("svmRadial")

# optimization
trControl <- trainControl(method='repeatedcv',number = 10, repeats = 2)
model2 <- train(Category ~.,
                data = train,
                method = 'svmRadial',
                metric = 'Accuracy',
                trControl = trControl,
                #tuneGrid=grid,
                tuneLength = 5
)

# show model 
model2

# plot(model3)
# show final model
model2$finalModel

# Model Evaluation
# Predict testing set
pred <- predict(model2, newdata = test) 
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$Category)
acc2 <- confusionMatrix(pred, test$Category)$overall['Accuracy']
print(acc2)

# Non-linear kernel - Polynomial

modelLookup("svmPoly")

# C for cost 

# optimization
trControl <- trainControl(method='repeatedcv',
                          number = 10, repeats = 2)
model3 <- train(Category ~.,
                data = train,
                method = 'svmPoly',
                metric = 'Accuracy',
                trControl = trControl,
                #tuneGrid=grid,
                tuneLength = 3
)

# show model 
model3

# plot(model4)
# show final model
model3$finalModel
model3$bestTune

# Model Evaluation
# Predict testing set
pred <- predict(model3, newdata = test) 
pred
# print(data.frame(test$mode, pred))

#Get the confusion matrix to see accuracy value and other parameter values 
confusionMatrix(pred, test$Category)
acc3 <- confusionMatrix(pred, test$Category)$overall['Accuracy']
print(acc3)
# print(data.frame(test$mode, pred))


result <- tibble(Model = c('SVM Linear',
                           'SVM Radial',
                           'SVM POly'),
                 Accuracy = c(acc1, 
                              acc2, 
                              acc3)
)

print(result %>% arrange(desc(Accuracy)))
#학습곡선
rpart_data <-
  learning_curve_dat(dat = df,
                     outcome = "Category",
                     test_prop = 1/4,
                     ## `train` arguments:
                     method = 'svmLinear2',
                     metric = "Accuracy")

p1 <- ggplot(rpart_data, aes(x = Training_Size, y = Accuracy, color = Data)) +
  geom_smooth(method = loess, span = .8) +
  theme_bw()
plot(p1)

#decision Tree 분류모델 생성성
train<-sample(1:616, 400)
rpartmod<-rpart(Category~., df[train,], method="class")
rpartpred<-predict(rpartmod, df[-train,], type='class')
print(confusionMatrix(rpartpred, df$Category[-train]))
