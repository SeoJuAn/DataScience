# 3번
mymapply = function(Func, ...) {
  length = vector()
  for(i in list(...)) {
    length <- c(length, length(i))
  }
  max <- max(length)
  boolean <- vector()
  for(i in length) {
    boolean <- c(boolean, max %% i)
  }
  if(is.element(FALSE, is.element(boolean, 0)) == TRUE) {
    print("인자들의 길이가 서로 배수관계에 있지 않습니다.")
    stop()
  }else {
    result <- vector()
    check <- list(...)
    for(i in 1:pivot) {
      if(typeof(check[[1]]) == "list") {
        result <- c(result, Func(...[[i]]))
      }
      else {
        p <- vector()
        for(lst in list(...)) {
          p <- c(p, lst[i])
        }
        result <- c(result, Func(p))
      }
    }
    return(result)
  }
}
c1 <- c(1,2,3,4,5,6)
c2 <- c(2,8,2,2,2,2)
c3 <- c(2,2,2,2,2,3)
m3 <- list(a=c(1:10), b=c(11:20))
m4 <- list(c=c(21:30), d=c(31:40))
c <- list(c1, c2, c3)
mymapply(typeof, c1, c2, c3)
mymapply(mean, c1, c2, c3)
mymapply(sum,c)
mymapply(typeof, c)

