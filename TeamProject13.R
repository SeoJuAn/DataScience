library(ggplot2)
library(Rlof)
library(mvoutlier)
library("dbscan")
library(factoextra)

data<-read.csv("서울 어린이집 공기질 정보.csv",header = TRUE, fileEncoding = "UCS-2LE")
data <- data[,-1]
data <- data[,-1]
data <- data[,-1]
data <- data[,-1]
data <- data[,-1]
data <- data[,-1]
data <- data[,-7]
data <- data[,-3]
data <- data[,-4]


print(str(data))


#통계적 방법을 통한 이상치 검출
#---------------------------------------------------------------------------

# #히스토그램
# hist(data$degree,
#      xlab = "degree",
#      main = "Histogram of degree"
# )
# 
# ggplot(data) +
#        aes(x = co2) +
#        geom_histogram(bins = 14, fill = "#0c4c8a")

# 
# ggplot(data) +
#   aes(x = "", y = degree) +
#   geom_boxplot(fill = "#0c4c8a") +
#   theme_minimal()
# 

# #box plot
# out <- boxplot.stats(data$hum)$out
# out_ind <- which(data$hum %in% c(out))
# #print(out_ind)
# 
# boxplot(data$hum,
#         ylab = "hum",
#         main = "child house Air Quality"
# )
# mtext(paste("Outliers: ", paste(out, collapse = ", ")))

#percentile 기법
# lower_bound <- quantile(data$degree, 0.025)
# print(lower_bound)
# upper_bound <- quantile(data$degree, 0.975)
# print(upper_bound)
# outlier_ind <- which(data$degree < lower_bound | data$degree > upper_bound)
# print(outlier_ind)
# print(data[outlier_ind, ])


#hampel filter 기능
# lower_bound <- median(data$degree) - 3 * mad(data$degree, constant = 1)
# print(lower_bound)
# upper_bound <- median(data$degree) + 3 * mad(data$degree, constant = 1)
# print(upper_bound)
# outlier_ind <- which(data$degree < lower_bound | data$degree > upper_bound)
# print(outlier_ind)


#LOF 방법을 통한 이상치 검출
#---------------------------------------------------------------------------


#DBSCAN 방법을 통한 이상치 검출
#---------------------------------------------------------------------------
dbscan::kNNdistplot(data,k=5)
abline(h=5,lty=2)
res.fps <- fpc::dbscan(data, eps = 5, MinPts = 20)
res.db <- dbscan::dbscan(data, 5,20)
fviz_cluster(res.fps, data, geom = "point")
print(which(res.fps$cluster==0))


# library(factoextra)
# data("multishapes")
# df <- multishapes[, 1:2]
# set.seed(123)
# km.res <- kmeans(data, 5, nstart = 25)
# fviz_cluster(km.res, data, frame = FALSE, geom = "point")
# 
# dbscan(data, eps, MinPts = 5, scale = FALSE, 
#        method = c("hybrid", "raw", "dist"))

# library("fpc")
# # Compute DBSCAN using fpc package
# set.seed(123)
# db <- fpc::dbscan(data, eps = 0.15, MinPts = 5)
# # Plot DBSCAN results
# plot(db, data, main = "DBSCAN", frame = FALSE)
# 
# library("factoextra")
# fviz_cluster(db, data, stand = FALSE, frame = FALSE, geom = "point")
