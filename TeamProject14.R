library(recommenderlab)
library(Amelia)
data_packages <- data(package = "recommenderlab")


data<-read.csv("df_modcloth2.csv",header = TRUE)
#print(summary(data))

#결측치 제거
data <- na.omit(data)
#결측치 제거 확인
print(colSums(is.na(data)))
missmap(data, col=c("yellow", "black"), legend = FALSE)

#데이터 탐색 
head(data)
str(data)
dim(data)

# 평점 매트릭스 생성
r <- as(data, "realRatingMatrix")
# 그룹별 행의 개수
print(mean(rowCounts(r)))
print(dim(r))
# 변환된 데이터 보기
image(r[1:18281,1:159])

# 샘플 추출 
set.seed(2029)
index <- sample(1:nrow(r), size = nrow(r) * 0.7)
train <- r[index, ]
test <- r[-index, ]

#1-10번째 행의 제품 평점 평균 
print(rowMeans(train[1:10,]))
# 아이템 평점의 분포  plot
hist(getRatings(train), main = "Distribution of ratings")
# 정규화된 아이템 평점의 분포  plot
hist(getRatings(normalize(train)), main = " Histogram of Normalized Product Ratings")
# 사용자별 평점 아이템 수 plot
hist(getRatings(train), main = "Distribution of ratings")


recomm_model <- Recommender(data = train, method = "UBCF")

#recomm_model <- Recommender(data = train, method = "IBCF")

print(recomm_model)

pred <- predict(recomm_model, newdata = test[1:10], n = 10)
pred_list <- sapply(pred@items, function(x) { colnames(r)[x] })
print(pred_list[1:10])

eval_sets <- evaluationScheme(data = train,
                              method = "split",
                              train = 0.9,
                              goodRating = 5,
                              given = 1)
getData(eval_sets, "train")


# 
# recomm_eval <- Recommender(data = getData(eval_sets, "train"),
#                            method = "UBCF")


recomm_eval <- Recommender(data = getData(eval_sets, "train"),
                           method = "IBCF")
recomm_eval

pred_eval <- predict(recomm_eval,
                     newdata = getData(eval_sets, "known"),
                     type = "ratings")
pred_eval

accuracy_eval <- calcPredictionAccuracy(x = pred_eval,
                                        data = getData(eval_sets, "unknown"))

print(accuracy_eval)
