#library(readxl)
#t1 <- read_excel(path="C:\\Users\\tjwnd\\OneDrive\\바탕 화면\\R WorkSpace\\titanic3.xls", sheet ="titanic3")

m1 <- matrix(1:24, nrow = 3, ncol = 8)

myapply <- function(Obj, Margin, Func, ...) { 
  result <- vector()
  if(Margin == 1){
    for(i in 1:nrow(Obj))
      result <- c(result, Func(Obj[i, ], ...))
    return(result)
  }
  if(Margin == 2){
    for(i in 1:ncol(Obj))
      result <- c(result, Func(Obj[ ,i], ...))
    return(result)
  }
}

mylapply <- function(obj, Func, ...){
  result <- list()
  list <- as.list(obj)
  
  for(i in 1:length(list)){
    result[i] <- Func(list[[i]])
    names(result)[[i]] <- names(list[i])
  }
  return(result)
}


mymapply <- function(Func,...){
  result <- vector()
  temp_matrix <- matrix()
  temp_vector <- vector()
  list1<-list()
  args <- list(...)
  for(i in 1:length(args)){
    temp <- as.list(args[[i]])
    for(j in 1:length(temp)){
      temp_vector <- c(temp_vector, Func(temp[[j]]))
    }
    list1[[i]]<-temp_vector
    temp_vector <- vector()
  }
  temp_matrix<-do.call(rbind,list1)

  if(length(args)==1){
    result <- as.vector(temp_matrix)
    print(result)
  }else{
    for(i in 1:ncol(temp_matrix)){
      result <- c(result, Func(temp_matrix[ ,i]))
    }
    print(result)
  }
}


test <- read.csv('test2.csv', header = TRUE, stringsAsFactors = FALSE, na.strings="")
a1 <- list(e=1:3, f=4:6)
b1 <- list(c=2:4, d=5:7)
c1 <- list(c=2:4, d=5:7)
f1 <- list(a=1:3)
d1 <- c(1,2)
e1 <- c(1,2,3)



result<-mymapply(max,a1,test)
print(mapply(max,a1,test))

