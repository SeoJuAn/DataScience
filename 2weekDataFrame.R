library(readxl)
titanic_data <- read_excel(path="C:\\Users\\tjwnd\\OneDrive\\바탕 화면\\R WorkSpace\\titanic3.xls", sheet ="titanic3")
t1<-titanic_data

t2 <- data.frame(name =t1$name,
                 pclass = factor(t1$pclass, levels = c(1, 2, 3)),
                 age = t1$age, 
                 parch = as.integer(t1$parch), 
                 sibsp = as.integer(t1$sibsp),
                 sex = factor(t1$sex, levels = c("female", "male")),
                 fare = t1$fare,
                 survived = factor(t1$survived, levels = c(0, 1)))
row_na = apply(t2,1,anyNA)
t3 <- t2[!row_na, ]

tm <- t3[t3$sex == "male", ]
tf <- t3[t3$sex == "female", ]

tm = tm[order(tm$age, -(tm$fare), tm$name), ]
tf = tf[order(tf$age, -(tf$fare), tf$name), ]


t4 = rbind(tm[1:100,], tf[1:100, ])


t5 <- data.frame(ID = 1:nrow(t4), t4, sum = t4$parch + t4$sibsp)

a <- sample(x = 1:10 , size = 5)

t6 = t5[sample(x=1:nrow(t5)),]

numeric_columns <- sapply(t6, is.numeric)
t7 <- t6[, numeric_columns]
print(t7)
result <- apply(t7, 2, sum, na.rm = TRUE)
print(result)