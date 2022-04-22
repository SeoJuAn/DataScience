Predict_Model <- function(test){
  predict <- c()
  print(test)
  for(i in 1:nrow(test)){
    if(test$stock[i]>=50000){
      predict <- c(predict, 1)
      next
    }else if(test$stock[i]<=20000){
      predict <- c(predict, 0)
      next
    }else if(test$degree[i]>=30){
      predict <- c(predict, 0)
      next
    }else if((test$degree[i]>=20 && test$degree[i]<=25) && test$hum[i] <=70){
      predict <- c(predict, 1)
      next
    }
    else{
      predict <- c(predict,0)
    }
  }
  
  return(predict)
}



test <- read.csv('test2.csv', header = TRUE, stringsAsFactors = FALSE, na.strings="")
predict <- Predict_Model(test)

cat("예측결과 : ", predict,"\n")
cat("실제결과 : ", test$result)

ComparisonTable <- table(predict, test$result)
print(ComparisonTable)

accuracy = (ComparisonTable[1,1]+ComparisonTable[2,2])/(ComparisonTable[1,1]+ComparisonTable[1,2]+ComparisonTable[2,1]+ComparisonTable[2,2])
accuracy <- accuracy * 100
cat("정확도 : ",accuracy,"%")