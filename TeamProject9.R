library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(mice)
library(GGally)
library(rpart)
library(randomForest)
library(prettyR)
library(rpart.plot)
library(rattle)
library(pROC)

require(randomForest)
require(mlbench)
#내장된 데이터여서 csv 파일이 따로 필요 없습니다.
data(Sonar)
levels(Sonar$Class)
Sonar$Class <- as.factor(Sonar$Class)

#층화추출
set.seed(10) # random seed
indexes = createDataPartition(Sonar$Class, p = .6, list = F)
train = Sonar[indexes, ]
test = Sonar[-indexes, ]

#렌덤포레스트 모델링
Sonar.rf<-randomForest(Class ~ ., data=Sonar, ntree=100, importance = T)
Sonar.rf
varImpPlot(Sonar.rf)

#렌덤 포레스트에 대한 ROC 모델링
rf.roc<-roc(Sonar$Class, Sonar.rf$votes[,2])
plot.roc(rf.roc,   
         col="royalblue",  
         print.auc=TRUE, 
         max.auc.polygon=TRUE,   
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")


Sonar.r<-randomForest(Class ~ ., data=train, ntree=100, importance = T)
pred_RF <- predict(Sonar.r, newdata = test, type = 'class')

print(confusionMatrix(test$Class, pred_RF))

#회귀곡선 모델 생성
Logistic = glm(Class ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+
                 V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+
                 V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+
                 V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+
                 V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+
                 V51+V52+V53+V54+V55+V56+V57+V58+V59+V60, 
               data = train, family = binomial())

#print(summary(Logistic))
pred_LOG <- predict(Logistic, newdata = test, type = 'response')
#print(pred)
PREDICTED_C = ifelse(pred_LOG > 0.5 , 'R' , 'M')
PREDICTED_C = as.factor(PREDICTED_C)
print(confusionMatrix(test$Class, PREDICTED_C))

#회귀곡선에 대한 ROC
ROC_LOG = roc(test$Class,pred_LOG)
plot.roc(ROC_LOG,   
         col="royalblue",  
         print.auc=TRUE, 
         max.auc.polygon=TRUE,   
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")

#Decision Tree 모델 생성
rpartmod<-rpart(Class ~., train, method = 'class')
#fancyRpartPlot(rpartmod)
rpartpred<-predict(rpartmod, test, type='prob')
pred_DT <- rpartpred[,2]
PREDICTED_C = ifelse(pred_DT > 0.5 , 'R' , 'M')
PREDICTED_C = as.factor(PREDICTED_C)

print(confusionMatrix(test$Class, PREDICTED_C))
#print(confusionMatrix(rpartpred, test$Class))
#Decision Tree에 대한 ROC
ROC_LOG = roc(test$Class,pred_DT)
plot.roc(ROC_LOG,
         col="royalblue",
         print.auc=TRUE,
         max.auc.polygon=TRUE,
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")

#nnet 모델 생성
set.seed(100)
getModelInfo()
#modelLookup("nnet")
getwd()
#maxit은 원래 500정도 돼야하는데 그러면 너무 오래걸려서 일단 50으로 해놨습니다.
#최적화하려면 500정도로 해야될 듯...
# optimization
trControl=trainControl(method='repeatedcv', number = 10, repeats = 2)
model = train(Class ~.,
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

print(model$finalModel)
X <- varImp(model$finalModel)
#plot(X)
# check the convergence of the final model
model$finalModel$convergence
# 1 if maxit reached, 0 otherwise

pred_NNET <- predict(model$finalModel, newdata = test, type = "raw")
print(pred_NNET)
PREDICTED_C = ifelse(pred_NNET > 0.5 , 'R' , 'M')
PREDICTED_C = as.factor(PREDICTED_C)

print(confusionMatrix(test$Class, PREDICTED_C))

#NNET 모델에 대한 ROC
ROC_NNET = roc(test$Class,pred_NNET)
plot.roc(ROC_NNET,
         col="royalblue",
         print.auc=TRUE,
         max.auc.polygon=TRUE,
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")


#SVM 모델 생성
names(getModelInfo("svm"))
modelLookup("svmLinear2")

# optimization
trControl <- trainControl(method='repeatedcv',number = 10, repeats = 2)
model1 <- train(Class ~.,
                data = train,
                probability = TRUE,
                method = 'svmLinear2',
                metric = 'Accuracy',
                tuneLength = 10,
                trControl = trControl
)

# Predict testing set
pred_SVM <- predict(model1, newdata = test,type = 'prob')
pred_SVM <- pred_SVM[,2]
PREDICTED_C = ifelse(pred_SVM > 0.5 , 'R' , 'M')
PREDICTED_C = as.factor(PREDICTED_C)

print(confusionMatrix(test$Class, PREDICTED_C))

#SVM 모델에 대한 ROC
ROC_SVM = roc(test$Class,pred_SVM)
plot.roc(ROC_SVM,
         col="royalblue",
         print.auc=TRUE,
         max.auc.polygon=TRUE,
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")