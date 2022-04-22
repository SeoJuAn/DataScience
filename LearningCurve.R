set.seed(1412)
#class_dat <- twoClassSim(1000)
wheat <- read.csv(file = "wheat-seeds.csv", header = TRUE)
wheat$class <- if_else(wheat$class == 1, "class1",if_else(wheat$class == 2,"class2","class3"))
wheat$class <- as.factor(wheat$class)
#print(class_dat$Class)
ctrl <- trainControl(classProbs = TRUE)
                     #summaryFunction = twoClassSummary)

set.seed(29510)
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
