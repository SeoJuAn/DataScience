library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)
library(klaR) # for NaiveBayes (=nb) function
library(prettyR)
library(rpart.plot)
library(rattle)

#Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
data<- iris
#data$Species <- if_else(data$Species==, "yes","no")
data$Species <- as.factor(data$Species)
#data <- data[,c(1,2,5)]
#print(data)


#독립검증

#일반적 독립
#독립
crosstab <- xtabs(formula = ~Sepal.Length+Sepal.Width, data = data)
chisq <- chisq.test(crosstab, correct = TRUE)
print(chisq)
#종속
crosstab <- xtabs(formula = ~Sepal.Length+Petal.Length, data = data)
chisq <- chisq.test(crosstab, correct = TRUE)
print(chisq)
#종속
crosstab <- xtabs(formula = ~Sepal.Length+Petal.Width, data = data)
chisq <- chisq.test(crosstab, correct = TRUE)
print(chisq)
#종속
crosstab <- xtabs(formula = ~Sepal.Width+Petal.Length, data = data)
chisq <- chisq.test(crosstab, correct = TRUE)
print(chisq)
#종속
crosstab <- xtabs(formula = ~Petal.Length+Petal.Width, data = data)
chisq <- chisq.test(crosstab, correct = TRUE)
print(chisq)
#p-value > 0.05 -> 귀무가설 채택(두 변수는 서로 독립임)


#Scatter plot을 이용한 독립 분석
gp1 <- ggplot(data=data_class1, aes(x=Sepal.Length, y=Sepal.Width))+geom_point()
plot(gp1)

gp2 <- ggplot(data=data_class1, aes(x=Sepal.Length, y=Petal.Length))+geom_point()
plot(gp2)

gp3 <- ggplot(data=data_class1, aes(x=Sepal.Width, y=Petal.Length))+geom_point()
plot(gp3)
#Scatter plot이 상하좌우 대칭이면 두 변수는 서로 독립



data_independance <- data[,c(1,2,5)]
data_dependance <- data[,c(1,3,5)]

#Naive Bayesian으로 예측
train <- sample(1:150, 100)
nb <- NaiveBayes(Species ~., data = data_dependance, subset = train)
predict<-predict(nb, data_dependance[-train,])$class
confusionMatrix <- table(data_dependance$Species[-train],predict)
print(confusionMatrix)
accuracy <- sum(confusionMatrix[row(confusionMatrix)==col(confusionMatrix)])/sum(confusionMatrix)
print("나이브 베이지안 분류 예측 정확도")
print(accuracy)

#Naive Bayesian 정확성 그래프
test <- data[-train,]
test$pred <- predict(nb, data[-train,])$class #예측된 분류 입력하기
p1 <- ggplot(test, aes(Species, pred, color = Species)) + geom_jitter(width = 0.2, height = 0.1, size=2)
labs(title="Confusion Matrix", y="Predicted", x="Truth")
plot(p1)

#print(data)

#decision Tree로 예측
rpartmod<-rpart(Species ~., data[train,], method="class")
fancyRpartPlot(rpartmod)
rpartpred<-predict(rpartmod, data[-train,], type='class')
print(confusionMatrix(rpartpred, data$Species[-train]))



#수치형 변수의 모델링(PDF 모델 생성)
data_class1 <- data[data$Species=='setosa',]
data_class2 <- data[data$Species == 'versicolor',]
data_class3 <- data[data$Species == 'virginica',]

mean1 <- mean(data_class1$Sepal.Length)
sd1 <- sd(data_class1$Sepal.Length)
d1 <- dnorm(sort(data$Sepal.Length),mean = mean1, sd = sd1)

mean2 <- mean(data_class2$Sepal.Length)
sd2 <- sd(data_class2$Sepal.Length)
d2 <- dnorm(sort(data$Sepal.Length),mean = mean2, sd = sd2)

mean3 <- mean(data_class3$Sepal.Length)
sd3 <- sd(data_class3$Sepal.Length)
d3 <- dnorm(sort(data$Sepal.Length),mean = mean3, sd = sd3)

plot(sort(data$Sepal.Length),d1, type = 'l',col = 'red')
lines(sort(data$Sepal.Length),d2,type = 'l',col = 'blue')
lines(sort(data$Sepal.Length),d3,type = 'l')

predict <- c()

for(i in data$Sepal.Length){
  percentage1 <- dnorm(i,mean = mean1, sd = sd1)
  percentage2 <- dnorm(i,mean = mean2, sd = sd2)
  percentage3 <- dnorm(i,mean = mean3, sd = sd3)
  if(percentage1 == max(percentage1, percentage2, percentage3)){
    predict <- c(predict, 'setosa')
  }else if(percentage2 == max(percentage1, percentage2, percentage3)){
    predict <- c(predict, 'versicolor')
  }else{
    predict <- c(predict, 'virginica')
  }
}
print("PDF분류예측 확률")
confusionMatrix <- table(data$Species,predict)
print(confusionMatrix)
accuracy <- sum(confusionMatrix[row(confusionMatrix)==col(confusionMatrix)])/sum(confusionMatrix)
print("정확도")
print(accuracy)

print("Decision Tree 분류예측 확률")
rpartmod<-rpart(Species ~., data[,c(1,5)][train,], method="class")
fancyRpartPlot(rpartmod)
rpartpred<-predict(rpartmod, data[,c(1,5)][-train,], type='class')
print(confusionMatrix(rpartpred, data$Species[-train]))
