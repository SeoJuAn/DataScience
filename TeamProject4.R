library(rpart)
library(e1071)
library(caret)
library(tidyverse)
library(rattle)
library(rpart.plot)

wheat <- read.csv(file = "wheat-seeds-selected.csv", header = TRUE)
wheat$class <- if_else(wheat$class == 1, "class1",if_else(wheat$class == 2,"class2","class3"))
wheat$class <- as.factor(wheat$class)

indexes = createDataPartition(wheat$class, p = .6, list = F) 
# tr set: 0.6
train = wheat[indexes, ]
test = wheat[-indexes, ]

fit <- rpart(class~., 
             data = train,
             parms = list(split = 'information'),
             cp = -1, 
             minsplit = 2,
             minbucket = 1,
             maxdepth = 5
             ) 

# prediction model
printcp(fit)

# plot the tree 
windows()
plot(fit)
text(fit, cex = 0.9, xpd = TRUE)
readline('Enter to resume ')
dev.off() # close window


windows()
fancyRpartPlot(fit)
readline('Enter to resume ')
dev.off() # close window

windows()
rpart.plot(fit)
readline('Enter to resume ')
dev.off() # close window

print(fit)
pred = predict(fit, test, type = "class", )
print(data.frame(test, pred))
print(confusionMatrix(test$class, pred))
readline('Enter to resume ')

fit <- rpart(class~., 
             data = train,
             parms = list(split = 'information'),
             cp = -1, 
             minsplit = 2,
             minbucket = 1,
             maxdepth = 5
             ) 

# prediction model 
printcp(fit)

# plot the tree 
windows()
plot(fit)
text(fit, cex = 0.9, xpd = TRUE)
readline('Enter to resume ')
dev.off() # close window

windows()
fancyRpartPlot(fit)
readline('Enter to resume ')
dev.off() # close window

windows()
rpart.plot(fit)
readline('Enter to resume ')
dev.off() # close window

# Evaluate the performance of the prediction model
pred = predict(fit, test, type = "class", )
print(data.frame(test, pred))
print(confusionMatrix(test$class,pred))




rpart_data <-
  learning_curve_dat(dat = wheat,
                     outcome = "class",
                     test_prop = 1/4,
                     ## `train` arguments:
                     method = "rpart",
                     metric = "Accuracy")
#trControl = ctrl)



p1 <- ggplot(rpart_data, aes(x = Training_Size, y = Accuracy, color = Data)) +
  geom_smooth(method = loess, span = .8) +
  theme_bw()
plot(p1)
#print(Accuracy)
