# Distance
# https://cran.r-project.org/web/packages/philentropy/vignettes/Distances.html
# https://rpubs.com/mrkm_a/ClusteringMethodsWithR

# Example data
# 3 vectors
Kor <- c(area  = 1, export = 8)
Usa <- c(area = 30,  export = 17)
Chn <- c(area = 20, export = 25)
x <- rbind(Kor, Usa, Chn)
x # data matrix

# Distance matrix
# compute the Euclidean Distance 
# with default parameters
x_dist <- dist(x, method = "euclidean") 
x_dist # distance matrix

# scale before distance
# % to max value: value / column_max
x_scaled <- scale(x, center = FALSE, apply(x, MARGIN = 2, FUN = max))
x_scaled
x_scaled_dist <- dist(x_scaled, method = "euclidean") 
x_scaled_dist

library(philentropy)
# philentropy::distance
distance(x, method = "euclidean") 

# names of implemented distance/similarity functions
getDistMethods()

# distance in stats package 
hc <- hclust(dist(x, method = "euclidean", diag = TRUE), 
             method = "centroid")
hc
plot(hc)

# cutting tree
# https://stat.ethz.ch/R-manual/R-patched/library/stats/html/cutree.html
hc <- hclust(dist(USArrests))

cutree(hc, k = 1:5)  # k = no of clusters
cutree(hc, h = 125)  # h = height

frame()
plot(hc)
abline(h = 125, col="red")

plot.new() # or frame()

plot(hc)
rect.hclust(hc, k = 2, border = 3:5)

# Compare the 2 and 4 grouping:
g24 <- cutree(hc, k = c(2,4))
table(grp2 = g24[,"2"], grp4 = g24[,"4"])

#  K-means clustering

data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data

# View the firt 3 rows of the data
head(df, n = 3)

# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

# Print the results
print(km.res)

# Determine K

# NbClust
library(NbClust)
nc <- NbClust(USArrests, min.nc=2, max.nc=15, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

# WithinSS - within sum of squares
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) # K = 1, Total SS 
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  }

wssplot(USArrests)

