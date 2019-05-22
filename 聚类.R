#加权主成分聚类程序
rm(list=ls())
setwd('E:/数统研究生/多元数据分析/总集/第二章/数据')
read.csv("医药银行20190303.csv",header=T)->DA
X_name<-DA[1:50,3:14]
X<-DA[1:50,4:14]
X<-na.omit(X)
X<-X[c(-30,-38),]
X_name<-na.omit(X_name)
X_name<-X_name[c(-30,-38),]
for (i in 1:ncol(X)){
  X[,i]<-as.numeric(X[,i])
}
d <- dist(scale(X))
########################################检验PCA是否适用
library(psych)
cortest.bartlett(cor(X), n = 120)
# KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy
kmo = function( data ){
  
  library(MASS)
  X <- cor(as.matrix(data))
  iX <- ginv(X)
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a)
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the
  # correlation matrix. That is the
  # negative of the partial correlations,
  # partialling out all other variables.
  
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  
  # Reporting the conclusion
  if (kmo >= 0.00 && kmo < 0.50){
    test <- 'The KMO test yields a degree of common variance
    unacceptable for FA.'
  } else if (kmo >= 0.50 && kmo < 0.60){
    test <- 'The KMO test yields a degree of common variance miserable.'
  } else if (kmo >= 0.60 && kmo < 0.70){
    test <- 'The KMO test yields a degree of common variance mediocre.'
  } else if (kmo >= 0.70 && kmo < 0.80){
    test <- 'The KMO test yields a degree of common variance middling.'
  } else if (kmo >= 0.80 && kmo < 0.90){
    test <- 'The KMO test yields a degree of common variance meritorious.'
  } else {
    test <- 'The KMO test yields a degree of common variance marvelous.'
  }
  
  ans <- list(  overall = kmo,
                report = test,
                individual = MSA,
                AIS = AIS,
                AIR = AIR )
  return(ans)
  
}    # end of kmo()
kmo(X)  
#########################################

row.names(X) <- X_name[,1]
pr<-princomp(X, cor=TRUE)
summary(pr, loadings=T)
#对数据做主成分分析

X<-as.matrix(X)
XX<-t(X)%*%X
eigen(XX)#求矩阵的特征值
X%*%eigen(XX)$vectors

pr<-princomp(X, cor=TRUE)
summary(pr, loadings=T)
pre<-predict(pr);pre
screeplot(pr,npcs=10,type=c("lines"))#主成分碎石图
biplot(pr)#缺省的是第一、二主成分散点图
biplot(pr,choices=3:4,scale=1)
biplot(pr,choices=5:6,scale=1)
#利用特征值计算累计贡献率
tezhz<-eigen(XX)$values
sum(tezhz[1:4])/sum(tezhz)
#结果输出到文件
write.csv(pre, "b3.csv", row.names = T)
data<-read.csv("b3.csv",head=TRUE)
PCAdata<-as.matrix(data[,2:5])
w<-tezhz[1:4]/sum(tezhz[1:4])
f<-PCAdata%*%w
F<-as.data.frame(cbind(PCAdata,f))
F<-cbind('名称'=X_name[,1],F)
write.csv(F, file = "得分.csv")
datax<-read.csv("得分.csv",head=TRUE)

#接下来，进行聚类：
X1<-read.csv("b3.csv",header=T)
#rownames(X)<-datax[,2]
rownames(X1)<-datax[,2]
X1<-X1[,2:5]
data1<-scale(X1)
write.csv(data1, file = "datarand.csv")

distance <- dist(data1,method ="canberra")  #lance距离
#source('E:/数统研究生/多元数据分析/总集/第二章/代码/马氏距离.R')
#distance <- dist.m(data1)#马氏距离的距离矩阵
#apply(distance,2,order)#查看每支股票的近邻
data.hc <- hclust(distance) #最长距离法
plot(data.hc, hang = -1) #绘画系谱图 
P <- rect.hclust(data.hc, k =4) #分为3类

data.hc <- hclust(distance,method="single") #最短距离法
plot(data.hc, hang = -1) #绘画系谱图 
P <- rect.hclust(data.hc, k = 3) #分为3类

data.hc <- hclust(distance,method="average") #类平均法
plot(data.hc, hang = -1) #绘画系谱图 
P <- rect.hclust(data.hc, k = 2) #分为2类

data.hc <- hclust(distance,method="centroid") #重心法
plot(data.hc, hang = -1) #绘画系谱图 
P <- rect.hclust(data.hc, k = 3) #分为3类

data.hc <- hclust(distance,method="median") #中间距离法
plot(data.hc, hang = -1) #绘画系谱图 
P <- rect.hclust(data.hc, k = 3) #分为3类

data.hc <- hclust(distance,method="ward.D") #离差平方和法
plot(data.hc, hang = -1) #绘画系谱图 
P <- rect.hclust(data.hc, k = 2) #分为2类

data.hc <- hclust(distance,method="mcquitty") #相似法
plot(data.hc, hang = -1) #绘画系谱图
P <- rect.hclust(data.hc, k = 2) #分为2类
