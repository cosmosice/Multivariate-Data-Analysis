rm(list = ls())
#*****************上证50(二选一)
#setwd('E:/数统研究生/多元数据分析/总集/第四章')
#read.csv('数据/DJI.csv') -> S
#DJI <- S[,51]
#S <- S[,-51]

#*****************道琼斯指数(二选一)
setwd('E:/数统研究生/多元数据分析/总集/第四章')
read.csv('数据/DJI.csv') -> S
S <- S[,-1]
DJI <- S[,16]
S <- S[,-16]

##改进
S <- scale(S)
DJI <- scale(DJI)
S <- S+abs(min(S))+0.001
DJI <- DJI+abs(min(DJI))+0.001

##KMO检查
source('代码/KMO.R') ->KMO
kmo(S)

#对数据做主成分分析
pr<-princomp(S, cor=TRUE)
summary(pr, loadings=T)

screeplot(pr,npcs=10,type=c("lines"))#主成分碎石图
library(ggplot2)
pca <- princomp(S)
principal.component <- pca$loadings[,1]
loadings <- as.numeric(principal.component)
ggplot(data.frame(Loading = loadings),aes(x = Loading,fill = 1))+geom_density()#载荷分布
stock.index <- predict(pca)[,1]
comparison <- data.frame(StockIndex = stock.index,DJI = DJI)#比较
ggplot(comparison,aes(x = StockIndex,y = DJI))+geom_point()+geom_smooth(method='lm',se=FALSE)
DS.lm <- lm(DJI~StockIndex,data = comparison)
summary(DS.lm)
