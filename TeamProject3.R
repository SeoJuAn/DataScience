library(ggplot2)
library(dplyr)


wine <- read.csv('wheat-seeds.csv', header = TRUE, stringsAsFactors = FALSE, na.strings="")
wine$class <- if_else(wine$class == 1, "1", if_else(wine$class == 2,"2","3"))

class1 <- wine[wine$class == "1",]
class2 <- wine[wine$class =="2",]

error_rates <- vector()
x <- vector()

for(i in sort(wine$groove)){
  temp1_M <- class1[class1$groove<i,]
  count1_M <- nrow(temp1_M)
  error_rate1 <- count1_M/nrow(class1)
  
  temp2_M <- class2[class2$groove>i,]
  count2_M <- nrow(temp2_M)
  error_rate2 <- count2_M/nrow(class2)
  
  error_rate <- error_rate1 + error_rate2
  error_rates <- c(error_rates,error_rate)
  x <- c(x, i)
}
index<-(which.min(error_rates))
print(min(error_rates))


 p1 = ggplot(wine, aes(x = groove, color = class, fill = class)) +
   geom_histogram(alpha=0.7, position="identity") +
   geom_vline(aes(xintercept = x[index]),
              color="blue", linetype="dashed", size=1)
 plot(p1)


# p2 <- ggplot(wine, aes(x = Proline, y = Hue,color = class)) +
#   geom_point(size = 6, shape = 20) +
#   stat_smooth(method = 'lm', se=F, color='black') 
#   
#   
# print(summary(lm(Hue ~ Proline, wine)))
# inclination <- summary(lm(Hue ~ Proline, wine))$coefficients[2,1] *-1
# intercept <- mean(wine$Hue)-mean(wine$Proline)*inclination
# #print(intercept)
# 
# p2 <- p2 +geom_abline(intercept = intercept, slope = inclination,color = 'blue',size = 1.5)
# plot(p2)
# 
# result <- class1$Proline*inclination+intercept < class1$Hue
# print(result)

