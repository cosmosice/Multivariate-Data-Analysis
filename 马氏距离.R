dist.m <- function(data){
  mashi <- function(a,b)
  {
    return (((a-b)%*% t(t(a-b))) / cov(a,b))
  }
  distance.m <- matrix(NA,nr=nrow(data)-1,nc=nrow(data)-1)
  for (i in 1:(nrow(data)-1)) {
    for(j in (i+1):nrow(data)){
      distance.m[(j-1),i] <- mashi(data[i,],data[j,])
    }}
  rownames(distance.m) <- rownames(data)[-1]
  colnames(distance.m) <- rownames(data)[-nrow(data)]
  return(distance.m)
}