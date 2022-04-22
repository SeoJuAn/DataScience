library(philentropy)
library(NbClust)
library(fpc)
library(ggplot2)
library(kohonen)
library(factoextra)

data<-read.csv("도로교통공단_가해운전자 연령층별 월별 교통사고 통계_20191231.csv", header = TRUE, fileEncoding = "euc-kr")
data <- data[,-1]
data <- data[,-1]

#print(data)
data_dist <- dist(data, method = "euclidean")
#print(data_dist) # distance matrix

#scale(형변환)
data <- scale(data)
#print(data)

getDistMethods()

hc <- hclust(dist(data))
print(hc)
plot(hc)

print(cutree(hc, k = 4))  # k = no of clusters
print(cutree(hc, h = 4))  # h = height

frame()
plot(hc)
abline(h = 4, col="red")


# plot.new() # or frame()
# 
# plot(hc)
rect.hclust(hc, k = 4, border = 3:6)

g24 <- cutree(hc, k = c(2,4))
print(table(grp2 = g24[,"2"], grp4 = g24[,"4"]))


#dc <- dbscan(data,eps=sqrt(10),MinPts = 4)
#dc <- dbscan(data,0.01) 
#print(dc)



data<-read.csv("도로교통공단_가해운전자 연령층별 월별 교통사고 통계_20191231.csv", header = TRUE)
data <- data[,-2]
train <- sample(1:96,66)
train_data <- list(x=as.matrix(data[train,-1]),age=as.factor(data[train,1]))
test_data <- list(x=as.matrix(data[-train,-1]),age=as.factor(data[-train,1]))


gr <- somgrid(xdim = 3, ydim = 5, topo = "hexagonal") #grid 갯수 및 모양 설정
ss <- supersom(train_data, gr, rlen = 200, alpha = c(0.05, 0.01)) #som 학습하기
plot(ss, type="changes")
plot(ss, type="count", main="Node Counts")
plot(ss, type="dist.neighbours", main = "SOM neighbour distances")
plot(ss, type="codes")

pred <- predict(ss, newdata = test_data)
#pred$prediction$age
print(table(pred$prediction$age,test_data$age))

############# k-means ##############
data<-read.csv("도로교통공단_가해운전자 연령층별 월별 교통사고 통계_20191231.csv", header = TRUE, fileEncoding = "euc-kr")
data <- data[,-2] # '월'열 제거

sapply(data, function(x) sum(is.na(x))) # 결측치 제거

data <- scale(data[2:6]) # 데이터 스케일링 (변수 표준화)

data <- data.frame(data)

##### NbClust #####
nc <- NbClust(data, min.nc=2, max.nc=15, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

## nbclust 결과: the best number of clusters is  2 

##### WithinSS - within sum of squares #####
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}
wssplot(data)
### wssplot 결과: 클러스터 수 2

# Compute k-means with k = 2
set.seed(123)
data.kmeans <- kmeans(data, centers = 2, nstart = 25)

print(data.kmeans)
data.kmeans$centers # 클러스터 센터
data.kmeans$withinss # 응집도. 같은 클러스터 안에 얼마나 데이터가 뭉쳐져있는지.(작을수록 좋다)
data.kmeans$betweenss # 분리도. 다른 클러스터간 얼마나 떨어져 있는지(클수록 좋다)

### 그래프 한글 깨짐 해결 par(family = "AppleGothic")

plot(data, col=data.kmeans$cluster) #변수쌍간 plot
data$cluster <- as.factor(data.kmeans$cluster)

qplot(사망자수, 사고건수, colour = cluster, data = data) + theme(text = element_text(size = 12, family = "AppleGothic"))
qplot(사망자수, 중상자수, colour = cluster, data = data) + theme(text = element_text(size = 12, family = "AppleGothic"))

###### 변수 선택 : 사망자수, 사고건수, 중상자수 ######

n_data<-subset(data,select=c(사고건수, 사망자수, 중상자수))
n_data.kmeans <- kmeans(n_data, centers = 2, nstart = 25)

n_data$cluster <- as.factor(n_data.kmeans$cluster)

n_data.kmeans$centers # 클러스터 센터
n_data.kmeans$withinss 
n_data.kmeans$betweenss

data_dist<-dist(data, method = "euclidean") #유클리디안 거리척도 사용

ndata_dist<-dist(n_data, method = "euclidean") #유클리디안 거리척도 사용

qplot(사망자수, 사고건수, colour = n_data.kmeans$cluster, data = n_data) + theme(text = element_text(size = 12, family = "AppleGothic"))
qplot(사망자수, 중상자수, colour = n_data.kmeans$cluster, data = n_data) + theme(text = element_text(size = 12, family = "AppleGothic"))

fviz_cluster(n_data.kmeans, data = n_data)



data<-read.csv("도로교통공단_가해운전자 연령층별 월별 교통사고 통계_20191231.csv", header = TRUE)
data <- data[,-1]
data <- data[,-1]

#print(data)
data_dist <- dist(data, method = "euclidean")
#print(data_dist) # distance matrix

#scale(형변환)
data <- scale(data)
#print(data)

getDistMethods()

hc <- hclust(dist(data))
print(hc)
plot(hc)

print(cutree(hc, k = 4))  # k = no of clusters
print(cutree(hc, h = 3.5))  # h = height

frame()
plot(hc)
abline(h = 3.7, col="red")


# plot.new() # or frame()
#
# plot(hc)
rect.hclust(hc, k = 4, border = 3:6)

g24 <- cutree(hc, k = c(2,4))
print(table(grp2 = g24[,"2"], grp4 = g24[,"4"]))


dc <- dbscan(data,eps=sqrt(10),MinPts = 4)
dc <- dbscan(data,0.01)
print(dc)



data<-read.csv("도로교통공단_가해운전자 연령층별 월별 교통사고 통계_20191231.csv", header = TRUE)
data <- data[,-2]
train <- sample(1:96,66)
train_data <- list(x=as.matrix(data[train,-1]),age=as.factor(data[train,1]))
test_data <- list(x=as.matrix(data[-train,-1]),age=as.factor(data[-train,1]))


gr <- somgrid(xdim = 3, ydim = 5, topo = "hexagonal") #grid 갯수 및 모양 설정
ss <- supersom(train_data, gr, rlen = 200, alpha = c(0.05, 0.01)) #som 학습하기

plot(ss, type="changes")

plot(ss, type="count", main="Node Counts")
plot(ss, type="dist.neighbours", main = "SOM neighbour distances")
plot(ss, type="codes")

pred <- predict(ss, newdata = test_data)
#pred$prediction$age
print(table(pred$prediction$age,test_data$age))


