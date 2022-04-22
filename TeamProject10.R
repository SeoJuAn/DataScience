library(e1071)
library(DAAG)
library(MASS)

df<-read.csv('BostonHousing.csv')
#print(df)
#print(colSums(is.na(df)))
scatter.smooth(x=df$B, y=df$MEDV, main=" ~ CRIM")
#상관관계 분석
#print(cor(df$TAX, df$MEDV))

#설명모델로써의 선형회귀 모델 생성
# build linear regression model on full data
linearMod <- lm(MEDV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE
                        +DIS+RAD+TAX+PTRATIO+B+LSTAT
                        , data=df)
print(linearMod)
print(summary(linearMod))
#F-test
#Residual standard error: 4.745 on 492 degrees of freedom
#Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7338 
#F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16
# -> F-test의 p-value가 0.05보다 작으므로 귀무가설 기각 => 위 모델은 유의미함.
# -> F-test의 R^2 값이 0.6보다 크므로 모델은 유의미함.

#T-test
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
#   CRIM        -1.080e-01  3.286e-02  -3.287 0.001087 ** 
#   ZN           4.642e-02  1.373e-02   3.382 0.000778 ***
#   INDUS        2.056e-02  6.150e-02   0.334 0.738288    
#   CHAS         2.687e+00  8.616e-01   3.118 0.001925 ** 
#   NOX         -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
#   RM           3.810e+00  4.179e-01   9.116  < 2e-16 ***
#   AGE          6.922e-04  1.321e-02   0.052 0.958229    
#   DIS         -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
#   RAD          3.060e-01  6.635e-02   4.613 5.07e-06 ***
#   TAX         -1.233e-02  3.760e-03  -3.280 0.001112 ** 
#   PTRATIO     -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
#   B            9.312e-03  2.686e-03   3.467 0.000573 ***
#   LSTAT       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***

# -> T-test의 p-value(Pr(>|t|))값의 의미는 해당 변수의 회귀계수가 0일 확률을 나타냄
#즉, Pr이 높을 수록 해당 변수가 종속변수와 무관할 가능성이 높음
# 0.05이상이면 귀무가설, 이하이면 귀무가설 기각
# 위에선 INDUS, AGE는 종속변수와 관련이 없고, RM, LSTAT는 관련이 크다고 볼 수 있다.
# 회귀계수 : t-vlaue. 부호가 양이면 비례관계, 음이면 반비례관계이다. 수가 클수록 종속변수 예측에 미치는 영향이 크다.




#stepwise-selection 실행
forward <- step(lm(MEDV ~1, df), scope = list(lower ~ 1,
                                      upper = ~CRIM+ZN+INDUS
                                      +CHAS+NOX+RM+AGE
                                      +DIS+RAD+TAX+PTRATIO
                                      +B+LSTAT), direction = "forward")
print(forward)
print(summary(forward))

backward <- step(lm(MEDV ~1, df), scope = list(lower ~ 1,
                                              upper = ~CRIM+ZN+INDUS
                                              +CHAS+NOX+RM+AGE
                                              +DIS+RAD+TAX+PTRATIO
                                              +B+LSTAT), direction = "backward")
print(backward)
print(summary(backward))

both <- step(lm(MEDV ~1, df), scope = list(lower ~ 1,
                                               upper = ~CRIM+ZN+INDUS
                                               +CHAS+NOX+RM+AGE
                                               +DIS+RAD+TAX+PTRATIO
                                               +B+LSTAT), direction = "both")
print(both)
print(summary(both))

#예측모델로써의 선형회귀 모델 생성
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(df), 0.8*nrow(df))  # row indices for training data
trainingData <- df[trainingRowIndex, ]  # model training data
testData  <- df[-trainingRowIndex, ]   # test data

lmMod <- lm(MEDV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE
                +DIS+RAD+TAX+PTRATIO+B+LSTAT
                , data=trainingData)

distPred <- predict(lmMod, testData)  # predict distance

#RMSE 계산
RMSE = function(m, o){
  return(sqrt(mean((m - o)^2))/mean(testData$MEDV))
}

print(RMSE(distPred, testData$MEDV))