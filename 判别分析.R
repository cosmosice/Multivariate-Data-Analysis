#支持向量机(SVM):
rm(list =ls())
setwd('E:/数统研究生/多元数据分析/总集/第三章/数据')
read.csv('训练数据.csv') -> train
read.csv('测试数据.csv',stringsAsFactors = F) -> test
train.subet = subset(train,select = c("Stock.gain","Stock.opened","Stock.class"),Stock.class %in% c("医药","银行"))
test.subet = subset(test,select = c("Stock.gain","Stock.opened","Stock.class"),Stock.class %in% c("医药","银行"))
library(e1071)
model <- svm(Stock.class ~ ., data = train,            
             method = "C-classification", kernel = "linear",              
             cost = 10, gamma = 0.1)
summary(model)

#调用plot函数绘制散点图

plot(x = train.subet$Stock.gain,y = train.subet$Stock.opened,col = train.subet$Stock.class,pch = 19)

#将支持向量用蓝色的圈注标出来
svm.model = svm(Stock.class ~ .,data = train.subet,kernel = "radial",cost = 1,scale = FALSE)
points(train.subet[svm.model$index,c(1,2)],col = "blue",cex = 2)
legend('topright', legend = unique(train.subet$Stock.class), pch = c(16,16), col = c('red','black'))

#加分隔线
w = t(svm.model$coefs) %*% svm.model$SV
b = -svm.model$rho
abline(a = -b/w[1,2],b=-w[1,1]/w[1,2],col = "red",lty = 5)

#正确率
p <- predict(svm.model,train.subet)
s <- ifelse(p == train.subet$Stock.class,1,0)
sum(s)/nrow(train.subet)
p <- predict(svm.model,test.subet)
s <- ifelse(p == test.subet$Stock.class,1,0)
sum(s)/nrow(test.subet)

#将惩罚因子设置为10000，重新训练一个SVM模型
plot(x = train.subet$Stock.gain,y = train.subet$Stock.opened,col = train.subet$Stock.class,pch = 19)
svm.model = svm(Stock.class ~ .,data = train.subet,kernel = "linear",cost = 10000,scale = FALSE)
points(train.subet[svm.model$index,c(1,2)],col = "blue",cex = 2)

w = t(svm.model$coefs) %*% svm.model$SV

b = -svm.model$rho

abline(a = -b/w[1,2],b=-w[1,1]/w[1,2],col = "red",lty = 5)

#正确率
p <- predict(svm.model,train.subet)
s <- ifelse(p == train.subet$Stock.class,1,0)
sum(s)/nrow(train.subet)
p <- predict(svm.model,test.subet)
s <- ifelse(p == test.subet$Stock.class,1,0)
sum(s)/nrow(test.subet)

#KSVM
library(kernlab)
stockmodel <- ksvm(Stock.class ~ ., data = train.subet,              
                  type = "C-bsvc",kernel = "rbfdot",                    
                  kpar = list(sigma = 0.1), C = 10,                    
                  prob.model = TRUE) 
stockmodel
predict(stockmodel, test.subet)#, type = "probabilities")

#Ksvm支持自定义核函数。如
k <- function(x, y) { (sum(x * y) + 1) * exp(0.001 * sum((x - y)^2)) }
class(k) <- "kernel"
data("promotergene")
gene <- ksvm(Class ~ ., data = promotergene, kernel = k, C = 10, cross = 5)#训练
gene

#对于二分类问题，可以对结果用plot()进行可视化。例子如下
x <- rbind(matrix(rnorm(120), , 2), matrix(rnorm(120, mean = 3), , 2))
y <- matrix(c(rep(1, 60), rep(-1, 60)))
svp <- ksvm(x, y, type = "C-svc", kernel = "rbfdot", kpar = list(sigma = 2))
plot(svp)


library(e1071)
set.seed(1234)
ind<-sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3)) #70%为训练集 30%为测试集
train<-iris[ind==1,]
test<-iris[ind==2,]
svm<-svm(train[,1:4],train[,5],type="C-classification",
         cost=10,kernel="radial",probability=TRUE,scale=FALSE)
pred<-predict(svm,test[,1:4],decision.values=TRUE)
table(pred,test[,5])

library(e1071)
model <- svm(Species ~ ., data = iris,            
             method = "C-classification", kernel = "radial",              
             cost = 10, gamma = 0.1)
summary(model)
plot(model, iris, Petal.Width ~
       Petal.Length, slice = list(Sepal.Width = 3,                              
                                  Sepal.Length = 4))
pre=predict(model, iris,type='class') 
table(pre,iris$Species) 

