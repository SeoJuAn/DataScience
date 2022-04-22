library(arules)
library(arulesViz)
library(wordcloud)
library(Amelia)
data<-read.csv("한국소비자원 소비자 피해구제 정보_20210413.csv", header = TRUE)

#sales에 대한 연관성 분석

# data <- data[,-1]
# data <- data[,-1]
# data <- data[,-6]
# na2_idx<-which(data$age=="불명")
# data$age[na2_idx]<-data$age[1]
# #print(str(data))
# 
# #연관분석 수행 (최소3개 이상의 아이템으로 지지도 0.003 신뢰도 0.5)
# rules<-apriori(data, parameter = list(minlen=3, support=0.003, confidence= 0.5),
#                appearance = list(rhs=c("sales=소셜커머스(쇼핑)", "sales=국내온라인거래", "sales=기타통신판매",
#                                        "sales=국제온라인거래","sales=TV홈쇼핑"), default = "lhs"))
# #결과확인
# inspect(sort(rules,by="lift"))
# 
# #그래프
# plot(head(sort(rules,by="lift"),5), method = "graph")

#계약해제,해지/위약금을 행한 성별 및 나이대 집단 연관성 분석
# data <- data[,-1]
# data <- data[,-1]
# data <- data[,-3]
# data <- data[,-3]
# 
# na2_idx<-which(data$age=="불명")
# data$age[na2_idx]<-data$age[1]
# print(str(data))
# 
# #연관분석 수행 (최소3개 이상의 아이템으로 지지도 0.003 신뢰도 0.5)
# rules<-apriori(data, parameter = list(minlen=4, support=0.01, confidence= 0.5),
#                appearance = list(rhs="note=계약해제.해지/위약금", default = "lhs"))
# #결과확인
# inspect(sort(rules,by="lift"))
# 
# #그래프
# plot(head(sort(rules,by="lift"),5), method = "graph")

# # #헬스장 이용권을 구매한 사람은 어떤 특징인가?
# data <- data[,-1]
# data <- data[,-1]
# data <- data[,-6]
# 
# na2_idx<-which(data$age=="불명")
# data$age[na2_idx]<-data$age[1]
# print(str(data))
# 
# #연관분석 수행 (최소3개 이상의 아이템으로 지지도 0.003 신뢰도 0.5)
# rules<-apriori(data, parameter = list(minlen=5, support=0.003, confidence= 0.1),
#                appearance = list(rhs="product=헬스장", default = "lhs"))
# #결과확인
# inspect(sort(rules,by="lift"))
# 
# #그래프
# plot(head(sort(rules,by="lift"),5), method = "graph")


# data <- data[,-1]
# data <- data[,-1]
# na2_idx<-which(data$age=="불명")
# data$age[na2_idx]<-data$age[1]
# 
# #연관분석 수행 (최소3개 이상의 아이템으로 지지도 0.003 신뢰도 0.5)
# rules<-apriori(data, parameter = list(minlen=4, support=0.01, confidence= 0.1),
#                appearance = list(rhs=c("note=계약해제.해지/위약금"), default = "lhs"))
# #결과확인
# inspect(sort(rules,by="lift"))
# # 결과해석 : 계약해제.해지/위약금을 요청한 product은 헬스장이 압도적으로 많았고 그 중 20대 여자임.
# #그래프
# plot(head(sort(rules,by="lift"),5), method = "graph")

data <- data[,-1]
data <- data[,-1]
na2_idx<-which(data$age=="불명")
data$age[na2_idx]<-data$age[1]

missmap(data, col=c("yellow", "black"), legend = FALSE)

#연관분석 수행 (최소3개 이상의 아이템으로 지지도 0.003 신뢰도 0.5)
rules<-apriori(data, parameter = list(minlen=4, support=0.001, confidence= 0.1),
               appearance = list(rhs=c("note=AS불만"), default = "lhs"))
#결과확인
inspect(sort(rules,by="lift"))
#그래프
plot(head(sort(rules,by="lift"),5), method = "graph")
#결과해석 :AS불만을 표한 품목은 스마트폰/TV로 전자제품이 가장 높은 지지도를 보였다. 또한 AS불만 피해구제를 신청한
#집단은 40,50대 남성이므로 해당 집단을 대상으로 설문 등을 통해서 전자제품 기업의 AS 서비스 향상을 도모할 수 있을 것이다.

data <- data[,-1]
data <- data[,-1]
na2_idx<-which(data$age=="불명")
data$age[na2_idx]<-data$age[1]

#연관분석 수행 (최소3개 이상의 아이템으로 지지도 0.003 신뢰도 0.5)
rules<-apriori(data, parameter = list(minlen=4, support=0.001, confidence= 0.1),
               appearance = list(rhs=c("product=항공여객운송서비스"), default = "lhs"))
#결과확인
inspect(sort(rules,by="lift"))
#그래프
plot(head(sort(rules,by="lift"),5), method = "graph")
#결과해석 : 항공여객운송서비스에서 계약해제.해지/위약금을 요구한 고객은 20대 여성이 많음
#따라서 항공사에서 경영시에 20대 여성의 계약해지 원인을 파악하고 그에 대한 해결 방안을 마련할 필요가 있음

data <- data[,-1]
data <- data[,-1]
na2_idx<-which(data$age=="불명")
data$age[na2_idx]<-data$age[1]

#연관분석 수행 (최소3개 이상의 아이템으로 지지도 0.003 신뢰도 0.5)
rules<-apriori(data, parameter = list(minlen=4, support=0.001, confidence= 0.1),
               appearance = list(rhs=c("product=건강(암·기타질병)보험"), default = "lhs"))
#결과확인
inspect(sort(rules,by="lift"))
#그래프
plot(head(sort(rules,by="lift"),5), method = "graph")
#결과해석 : 건강(암, 기타질병) 보험에 관하여 부당행위 즉, 보험사기에 연류된 대상자는 50대의 여성인 경우가 많음을 확인할 수 있다.
#따라서 보험 사기 예방 캠페인의 주 타겟층을 해당 집단으로 설정하거나 보험 사기 조사 등에도 활용할 수 있을 것이다.
