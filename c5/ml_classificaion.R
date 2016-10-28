#ml classificaion
#f
#5.1
###error 
Fold <- function(Z=10,w,D,seed=7777)
{
n = nrow(w);d = 1:n; dd=list()
e=levels(w[,D]);T=length(e)
set.seed(seed)
for(i in 1:T){
d0=d[w[,D]==e[i]];j=length(d0)
ZT=rep(1:Z,ceiling(j/Z))[1:j]
id = cbind(sample(ZT,length(ZT)),d0);dd[[i]]=id}
mm=list()
for(i in 1:Z){u=NULL;
for(j in 1:T)u=c(u,dd[[j]][dd[[j]][,1]==i,2])
mm[[i]]=u}
return(mm)
}
###error occur

w=read.csv("CTG_RAW.csv")[,c(7:11,13:28,39,40)]
names(w)
F=21:23; for(i in F)w[,i]=factor(w[,i])
D=23;Z=10;n=nrow(w);mm=Fold(Z,w,D,8888)
str(w)

#5.2 de
#5.2.1 fitting the entire data-set
library(rpart.plot)
(a=rpart(NSP~.,w))
rpart.plot(a,type=2,extra=4)

wp=predict(a,w,type="class")
(z=table(w[,D],wp))   # row -actual value,column- predict value
sum(w[,D]!=wp)/nrow(w)

library(rpart)
E=rep(0,Z)
for(i in 1:Z){m = mm[[i]];
    n1 = length(m);a=rpart(NSP~.,w[-m,])
    E[i]=sum(w[m,D]!=predict(a,w[m,],type="class"))/n1
}
mean(E)

#5.3 adaboost classification
#5.3.1 fitting the entire data set
library(adabag)
set.seed(4410)
a=boosting(NSP~. ,w)
wp=predict(a,w)$class
(z = table(w[,D],wp))
sum(w[,D]!=wp)/nrow(w)
barplot(a$importance,cex.name=.6)

#average residual error rate
set.seed(1010)
E=rep(0,Z)
for(i in 1:Z){
print(i)
m=mm[[i]]
n1=length(m)
a=boosting(NSP~.,w[-m,])
E[i]=sum(as.character(w[m,D])!=predict(a,w[m,])$class)/n1

}
mean(E)


#5.4 bagging
#5.4.1 fitting the entire data set
set.seed(1010);D=23
a=bagging(NSP~.,w)
wp=predict(a,w)$class
(z=tables(w[,D],wp))
sum(w[,D]!=wp)/nrow(w)
barplot(a$importance,cex.names = 0.8)
#
library(adabag)
t<-Sys.time()

w=read.csv("CTG_RAW.csv")[,c(7:11,13:28,39,40)]
names(w)
F=21:23; for(i in F)w[,i]=factor(w[,i])
D=23;Z=10;n=nrow(w);mm=Fold(Z,w,D,8888)
str(w)
set.seed(1010)
E=rep(0,Z)
for(i in 1:Z){m=mm[[i]]
n1=length(m)
a=bagging(NSP~.,w[-m,])
E[i]=sum(as.character(w[m,D])!=predict(a,w[m,])$class)/n1
}
Sys.time()-t
mean(E)
# 5.5 random forests
library(randomForest)
#fitting the entire data
w=read.csv("CTG_RAW.csv")[,c(7:11,13:28,39,40)]
names(w)
F=21:23; for(i in F)w[,i]=factor(w[,i])
set.seed(1010)
a=randomForest(NSP~.,w,importance=TRUE,proximity=TRUE)
wp=predict(a,w)
(z=table(w[,D],wp))
class(z)
typeof(z)
(E0=sum(z)-sum(diag(z))/sum(z))
sum(w[,D]!=wp)/nrow(w)
#barplot(a$importance,cex.names = 0.8)
par(mfrow=c(3,1))
matplot(importance(a)[,1:3],type="o",pch=1:3,lty=1:3,col=1,
        xlab="Variable Number", ylab="Importance")
title("variable importance for 3 levels of response")
legend("topleft",legend = paste("NSP=",1:3,sep=""),
       pch=1:3,lty=1:3,col=1)
barplot(importance(a)[,4],cex.names = 0.6)
title("variable importance according to mean decrease accuracy")
par(mfrow=c(1,1))

a #see the oob

#5.5.2 
set.seed(1010)
E=rep(0,Z)
for(i in 1:Z){m=mm[[i]]
n1=length(m)
a=randomForest(NSP~.,data=w[-m,])
E[i]=sum(w[m,D]!=predict(a,w[m,]))/n1  
}
mean(E)

#5.6 SVM
#5.6.1 the whole data's fitting
#e1071
library(e1071)
a=svm(NSP~.,data=w,kernal="sigmoid")
wp=predict(a,w)
(z=table(w[,D],wp))
(E=(sum(z)-sum(diag(z)))/sum(z))

#kernlab
library(kernlab)
a=ksvm(NSP~.,data=w)
wp=predict(a,w)
(z=table(w[,D],wp))
(E=(sum(z)-sum(diag(z)))/sum(z))

# cross testing
#e1071

E=rep(0,Z)
for(i in 1:Z){m=mm[[i]]    
n1=length(m)
a=svm(NSP~.,data=w[-m,],kernal="sigmoid")   #warning
E[i]=sum(w[m,D]!=predict(a,w[m,]))/n1  
}
mean(E)

#kernlab
E=rep(0,Z)                       #wwarning
for(i in 1:Z){m=mm[[i]]
n1=length(m)
a=ksvm(NSP~.,data=w[-m,])
E[i]=sum(w[m,D]!=predict(a,w[m,]))/n1  
}
mean(E)


#5.7 knn
#5.7.1 fitting the entire data-set
library(kknn)
a=kknn(NSP~.,k=6,train=w,test=w)   #page 88 ,3th edition
(z=table(w[,D],a$fitted.values))
(E=(sum(z)-sum(diag(z)))/sum(z))

#5.7.3 cross testing
E=rep(0,Z)                       #wwarning
for(i in 1:Z){ m=mm[[i]]
n1=length(m)
a=kknn(NSP~.,k=6,train=w[-m,],test=w[m,])
E[i]=sum(w[m,D]!=a$fitted.values)/n1 }
mean(E)

#5.8 artificial neuro net
#5.8.1 fitting the whole
library(nnet)
library(caret)
# this step
mygrid=expand.grid(.decay=c(0.5,0.1),.size=c(2:8))
nnetfit=train(NSP/max(w[,D])~.,data=w,method="nnet", maxit=1000,tuneGrid=mygrid, trace=FALSE)
print(nnetfit)


a=nnet(NSP~.,data=w,subset=1:n,
       size=2 ,rang=0.01,decay=5e-4,maxit=200   #when size=7
       )
wp=predict(a,w[m,],type="class")
sum(w[m,D]!=wp)/length(m)

#cross test
library(nnet)
E=rep(0,Z)
for(i in 1:Z)
{ m=mm[[i]];mc=setdiff(1:n,m)
a=nnet(NSP~.,data=w,subset=mc,size=2,
       rang=0.1,decay=0.01,maxit=200)
E[i]=sum(w[m,D]!=predict(a,w[m,],type="class"))/length(m) }

mean(E)








