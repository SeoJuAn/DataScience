library(recommenderlab)
library(Amelia)
library(dplyr) 
data_packages <- data(package = "recommenderlab")

# user_artists 데이터셋 로드 (userid, artistid, weight쓸 예정)
user_artists = read.table("user_artists.dat",sep="\t",fileEncoding = "UTF-8",stringsAsFactors = F,comment.char = "",quote="",header = T)
user_artists <- user_artists[1:1000,]
data <- as.data.frame.matrix(user_artists)

sorted_data <- arrange(data,data$weight)
sorted_data$weight <- if_else(sorted_data$weight <= 142, 1,
                              if_else(sorted_data$weight <= 260,2,
                                      if_else(sorted_data$weight <= 514,3,
                                              if_else(sorted_data$weight <= 963,4,
                                                  5))))



r <- as(sorted_data, "realRatingMatrix")
model_UBCF <- Recommender(r,method="UBCF",parameter="Cosine")
hist(getRatings(r), main = "Distribution of ratings")

UBCFlist <- predict(model_UBCF, r[1], n=5)
print(as(UBCFlist,"list"))

accuracy_eval <- calcPredictionAccuracy(x = UBCFlist,
                                        data = getData(r, "unknown"),
                                        byUser = TRUE) # byUser = TRUE : 각 사용자들에 대한 모델의 정확도가 계산