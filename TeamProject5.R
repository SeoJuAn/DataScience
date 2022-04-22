library(rpart)
library(caret)
library(e1071)
library(rattle)
library(tidyverse)
library(Amelia)
library(rpart.plot)

df<-read.csv('in-vehicle-coupon-recommendation.csv'
             , stringsAsFactors = F
             , na.strings = c("","NA", "Unknown", "NULL"))
df$Y <- if_else(df$Y == 1, "yes","no")
df$destination <- as.factor(df$destination)
df$passanger <- as.factor(df$passanger)
df$weather <- as.factor(df$weather)
df$temperature <- as.factor(df$temperature)
df$temperature<-ordered(df$temperature,levels = c(30,55,80))



df$time <- as.factor(df$time)
df$coupon <- as.factor(df$coupon)
df$expiration <- as.factor(df$expiration)
df$gender <- as.factor(df$gender)
df$age <- as.factor(df$age)
df$age<-ordered(df$age,levels = c('below21',21,26,31,36,41,46,'50plus'))


df$maritalStatus <- as.factor(df$maritalStatus)
df$has_children <- as.factor(df$has_children)
df$education <- as.factor(df$education)
#df$education <- ordered(df$education, levels = c('Some High School', 'High School Graduate', 'Some college - no degree', 'Associates degree', 'Bachelors degree', 'Graduate degree (Masters or Doctros)'))
df$occupation <- as.factor(df$occupation)
df$income <- as.factor(df$income)

df$Bar <- as.factor(df$Bar)
df$CoffeeHouse <- as.factor(df$CoffeeHouse)
df$CarryAway <- as.factor(df$CarryAway)
df$CarryAway<-ordered(df$CarryAway,levels = c('never','less1','1~3','4~8','gt8'))


df$RestaurantLessThan20 <- as.factor(df$RestaurantLessThan20)
df$RestaurantLessThan20<-ordered(df$RestaurantLessThan20,levels = c('never','less1','1~3','4~8','gt8'))

df$Restaurant20To50 <- as.factor(df$Restaurant20To50)
df$Restaurant20To50<-ordered(df$Restaurant20To50,levels = c('never','less1','1~3','4~8','gt8'))

df$toCoupon_GEQ5min <- as.factor(df$toCoupon_GEQ5min)
df$toCoupon_GEQ15min <- as.factor(df$toCoupon_GEQ15min)
df$toCoupon_GEQ25min <- as.factor(df$toCoupon_GEQ25min)
df$direction_same <- as.factor(df$direction_same)
df$direction_opp <- as.factor(df$direction_opp)
df$Y <- as.factor(df$Y)






#층화추출 과정
intrain<-createDataPartition(y=df$Y, p=0.7, list=FALSE) 
train<-df[intrain, ]
test<-df[-intrain, ]

#EDA 과정
#print(str(df))
#Y의 분포량 확인
print(prop.table(table(df$Y)))
tb1 <- table(df$Y)
barplot(tb1)

#결측치 확인
#print(is.na(df))
print('--------------결측치 제거 전---------------')
print(colSums(is.na(df)))
missmap(df, col=c("yellow", "black"), legend = FALSE)

#결측치를 최빈값으로 대체(str이 있기 때문에 중앙값이나 평균으로는 불가능)
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
df$Bar[is.na(df$Bar)] <- getmode(df$Bar)
df$CoffeeHouse[is.na(df$CoffeeHouse)] <- getmode(df$CoffeeHouse)
df$CarryAway[is.na(df$CarryAway)] <- getmode(df$CarryAway)
df$RestaurantLessThan20[is.na(df$RestaurantLessThan20)] <- getmode(df$RestaurantLessThan20)
df$Restaurant20To50[is.na(df$Restaurant20To50)] <- getmode(df$Restaurant20To50)

#결측치 재확인
#print(is.na(df))
print('--------------결측치 제거 후---------------')
print(colSums(is.na(df)))
missmap(df, col=c("yellow", "black"), legend = FALSE)
#tb2 <- table(df$CarryAway)
#barplot(tb2)

#Pre-Pruning 과정
train.control <- trainControl(
                          method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE
                          )
rpartFit1 <- train(Y~., data = train,
                   method = "rpart2",
                   tuneLength = 10,
                   trControl = train.control,
                   na.action = na.omit,
                   metric = "ROC",
                   )
print(rpartFit1)
plot(rpartFit1)

fancyRpartPlot(rpartFit1$finalModel)
pre_model <- rpartFit1$finalModel

print(pre_model)
best_maxdepth <- rpartFit1$bestTune$maxdepth[1]

model <- rpart(Y~., train, control = rpart.control(maxdepth = best_maxdepth))
pred = predict(model, test, type = "class", )
#print(data.frame(test, pred))
print(confusionMatrix(test$Y, pred))

print("---------------post-pruning-------------------")

#Post-Pruning 과정
#full tree
print("------------full tree--------------")
rpartmod<-rpart(Y~., train, method="class")
fancyRpartPlot(rpartmod)
rpartpred<-predict(rpartmod, test, type='class')
print(confusionMatrix(rpartpred, test$Y))

print("------------pruned--------------")
printcp(rpartmod)
plotcp(rpartmod)
ptree<-prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree)


rpartpred<-predict(ptree, test, type='class')
print(confusionMatrix(rpartpred, test$Y))

#rpart.rules 추출
print(rpart.rules(pre_model))
