rm(list = ls())
setwd('E:/数统研究生/多元数据分析/总集/第五章/数据')
#因子分析的主成分解
factor=function(S, m){
  p<-nrow(S)
  diag_S<-diag(S)
  sum_rank<-sum(diag_S)
  rowname<-paste("X", 1:p, sep="")
  colname<-paste("Factor", 1:m, sep="")
  A<-matrix(0, nrow=p, ncol=m,
            dimnames=list(rowname, colname))
  eig<-eigen(S)
  for (i in 1:m)
    A[,i]<-sqrt(eig$values[i])*eig$vectors[,i]
  h<-diag(A%*%t(A))
  rowname<-c("SS loadings","Proportion Var","Cumulative Var")
  B<-matrix(0, nrow=3, ncol=m,
            dimnames=list(rowname, colname))
  for (i in 1:m){
    B[1,i]<-sum(A[,i]^2)
    B[2,i]<-B[1,i]/sum_rank
    B[3,i]<-sum(B[1,1:i])/sum_rank
  }
  list(loadings=A,
       var=cbind(common=h, spcific=diag_S-h), B=B)
}
read.csv("医药20190309.csv",header=T)->C
Test<-C[,c("营业收入","净利润","净资产","总现金流","权益比","营业成本")]
R=cor(scale(Test));R
eigen(R)
fa<-factor(R,m=3); fa
cbind(fa$loadings,fa$var)
vm1<-varimax(fa$loadings, normalize = F); vm1#方差最大的正交旋转

#计算得分
coef <- MASS::ginv(R)%*%vm1$loadings
score <- as.matrix(Test) %*% as.matrix(coef)
weight <- matrix(c(fa$B[2,1],fa$B[2,2],fa$B[2,3]), nrow = 1)
weight <- weight/fa$B[3,3]
TotalScore <- score%*%t(weight)
rownames(TotalScore) <- C[,2]
TS.lm <- lm(TotalScore~Test$营业成本)
plot(Test$营业成本,TotalScore)
abline(TS.lm,col ='red')
#稀疏因子分析
#install.packages("elasticnet")

library(elasticnet)

out1<-spca(R,K=3,para=c(0.06,0.16,0.1),type="Gram",sparse="penalty",trace=TRUE,use.corr = true, lambda = 1e-06);out1
out2<-spca(R,K=3,para=c(0.6,0.5,0.23),type="Gram",sparse="penalty",trace=TRUE,use.corr = true, lambda = 1e-06);out2
out1$loadings
out2$loadings
