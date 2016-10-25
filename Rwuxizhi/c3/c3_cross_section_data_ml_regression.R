#c3 cross section data 
w=read.csv("mg.csv",header = FALSE)
names(w)=c("y","x1","x2","x3","x4","x5","x6")
a=lm(y~.,w)
cor(w)
plot(w);pairs(w)

CV=function(n, Z=10, seed=888)
{
  z=rep(1:Z,ceiling(n/Z))[1:n];set.seed(seed);z=sample(z,n)
  mm=list(); for(i in 1:Z) mm[[i]]=(1:n)[z==i];return(mm)
}

w=read.csv("mg.csv",header = FALSE)
names(w)=c("y","x1","x2","x3","x4","x5","x6");
n=nrow(w);Z=10;mm=CV(n,Z);D=1 #the dependent varibles

#the NMSE of simple linear regression
MSE=rep(0,Z)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
  a=lm(y~.,w[-m,])
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
}
mean(MSE)

#3.2 decision tree

library(rpart.plot)
a=rpart(y~.,w)
a
MSE=rep(0,Z)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
a=rpart(y~.,w[-m,])
MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
}
mean(MSE)

#3.3 boosting regression

library(mboost)
#a=rpart(y~.,w)
#a
MSE=rep(0,Z)
set.seed(1010)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
a=mboost(y~btree(x1)+btree(x2)+btree(x3)+btree(x4)+btree(x5)+btree(x6),data=w[-m,])
MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
}
mean(MSE)

#3.4 bagging

library(ipred)
#a=rpart(y~.,w)
#a
MSE=rep(0,Z)
set.seed(1010)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
a=bagging(y~.,data=w[-m,])
MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
}
mean(MSE)


#3.5 random forest

library(randomForest)
#a=rpart(y~.,w)
#a
MSE=rep(0,Z)
set.seed(1010)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
a=randomForest(y~.,data=w[-m,])
MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
}
mean(MSE)

#3.6 SVR
 #rminer
library(rminer)
#a=rpart(y~.,w)
#a
MSE=rep(0,Z)
set.seed(1010)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
a=fit(y~.,data=w[-m,],model="svm")
MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
}
mean(MSE)

#e1071
library(e1071)
#a=rpart(y~.,w)
#a
MSE=rep(0,Z)
set.seed(1010)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
a=svm(y~.,w[-m,])
MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
}
mean(MSE)

#e1071
library(e1071)
#a=rpart(y~.,w)
#a
MSE=rep(0,Z)
set.seed(1010)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
a=svm(y~.,w[-m,])
MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
}
mean(MSE)

#kernlab
library(kernlab)
#a=rpart(y~.,w)
#a
MSE=rep(0,Z)
set.seed(1010)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
a=ksvm(y~.,w[-m,],model="svm")
MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
}
mean(MSE)

# artificial neural networks
#nnet
library(nnet)
library(caret)
set.seed(1010)

mygrid=expand.grid(.decay=c(0.5,0.1),.size=c(4,5,6))
nnetfit=train(y/max(w[,D])~.,data=w,method="nnet", maxit=1000,tuneGrid=mygrid, trace=FALSE)
print(nnetfit)
#can using sting order to get the size and decay from the result

MSE=rep(0,Z)
set.seed(1010)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
a=nnet(y/max(w[,D])~.,data=w[-m,], size=4,decay=0.1)
MSE[i]=mean((w[m,D]-predict(a,w[m,])*max(w[,D]))^2)/M
}
mean(MSE)

#neuralnet
library(neuralnet)
ny=(1:ncol(w))[-D]
nn1=paste(names(w)[D],"~",names(w)[ny[1]],seq="")
for(i in (1:ncol(w))[-D][-1]) nn1 = paste(nn1,"+",names(w)[i],seq="")
v=w
v[,D]= v[,D]/max(w[,D])
#------------------------------------------
MSE=rep(0,Z)
set.seed(1010)
for(i in 1:Z)
{ m=mm[[i]]; M=mean((w[m,D]-mean(w[m,D]))^2)
a=neuralnet(nn1,data=v[-m,], err.fct ="sse",hidden = 4,linear.output = FALSE)
MSE[i]=mean((w[m,D]-compute(a,v[m,-D])$net.result*max(w[,D]))^2)/M
}
mean(MSE)

#discussion
w <- read.csv("mg.csv")
names(w)=c("y","x1","x2","x3","x4","x5","x6")
m <- sample(1:1385, ceiling(1385/2))

library(randomForest);n=100;NMSE=rep(0,n)->NMSEO
set.seed(1010)
for(i in 1:n)
{
  A= randomForest(y~.,data=w[-m,],ntree=i)
  y0=predict(A,w[-m,]); y1=predict(A,w[m,])
  NMSEO[i] = mean((w$y[-m]-y0)^2)/mean((w$y[-m]-mean(w$y[-m]))^2)
  NMSE[i] = mean((w$y[m]-y1)^2)/mean((w$y[m]-mean(w$y[m]))^2)
}
#----------------------------------------------------------------------
D=1
library(neuralnet);set.seed(1010);n1=40;nmse=rep(0,n1)->nmse0
ny=(1:ncol(w))[-D]
nn1=paste(names(w)[D],"~",names(w)[ny[1]],seq="")
for(i in (1:ncol(w))[-D][-1]) nn1 = paste(nn1,"+",names(w)[i],seq="")
v=w
v[,D]= v[,D]/max(w[,D])

for(i in (1:ncol(w))[-D][-1]) nn1 = paste(nn1,"+",names(w)[i],seq="")
for(i in 1:n1)
{
  a = neuralnet(nn1, data=v[-m,],err.fct = "sse", hidden=i,linear.output = FALSE)
  y0=compute(a,v[-m,-D])$net.result*max(w[,D])
  y1=compute(a,v[m,-D])$net.result*max(w[,D])
  nmse0[i] = mean((w$y[-m]-y0)^2)/mean((w$y[-m]-mean(w$y[-m]))^2)
  nmse[i] = mean((w$y[m]-y1)^2)/mean((w$y[m]-mean(w$y[-m]))^2)
 }

######
par(mfrow=c(1,2))
plot(1:n,NMSE,type="l",ylim=c(min(NMSE,NMSEO),max(NMSE,NMSEO)),
                     xlab="number of trees",ylab="NMSE",
     main="Random Forestes: NMSE",lty=1)
lines(1:n, NMSEO)
legend("topright", lty=1:2,c("traiing set","testing set"))
plot(1:n1,nmse,type="l",ylim=c(min(nmse,nmse0),max(nmse,nmse0)),
     xlab="number of the units in the hidden layer",ylab="NMSE",main="NNET: NMSE",lty=2
     ); lines(1:n1,nmse0)
legend("topright", lty=1:2,c("traiing set","testing set"))

     





