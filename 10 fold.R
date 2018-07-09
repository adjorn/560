data=read.csv("C:\\Users\\yu deng\\Desktop\\统计作业\\final\\2\\hepatitis.data",header=F)
data
data[,1]=data[,1]-1##0 denote die,1 denote alive.
acc=function(t){
as.matrix(t)
n=ncol(t)
a=matrix(numeric(2),ncol=1)
if(n==1){t=cbind(a,t)}else{t=t}
return((t[1,1]+t[2,2])/sum(t))}
  
sn=function(t){
as.matrix(t)
n=ncol(t)
a=matrix(numeric(2),ncol=1)
if(n==1){t=cbind(a,t)}else{t=t}
return(t[2,2]/(t[2,1]+t[2,2]))}

sp=function(t){
as.matrix(t)
n=ncol(t)
a=matrix(numeric(2),ncol=1)
if(n==1){t=cbind(a,t)}else{t=t}
return(t[1,1]/(t[1,1]+t[1,2]))}

ppv=function(t){t
as.matrix(t)
n=ncol(t)
a=matrix(numeric(2),ncol=1)
if(n==1){t=cbind(a,t)}else{t=t}
return(t[2,2]/(t[1,2]+t[2,2]))}

npv=function(t){
as.matrix(t)
n=ncol(t)
a=matrix(numeric(2),ncol=1)
if(n==1){t=cbind(a,t)}else{t=t}
return(t[1,1]/(t[1,1]+t[2,1]))}

library(rpart)

##20 trials 10-fold cross validation
acc.d=numeric(20)
sn.d=numeric(20)
sp.d=numeric(20)
ppv.d=numeric(20)
npv.d=numeric(20)

n=nrow(data)
s=floor(n/10)##get 1/10 size for test data.

for(j in 1:20){
p=NULL
k=1:n
b=sample(k,size=n,replace=F)
##divide the test index into 2 classes of groups. In one class each group has 15 data. The orders have 15 data.
t1.ind=matrix(b[1:(s*5)],nrow=5,byrow=T)
t2.ind=matrix(b[(s*5+1):n],nrow=5,byrow=T)


formula=V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20
for(i in 1:5){
t.ind=t1.ind[i,]##index of test data
d.ind=k[-t.ind]##index of trainging data
fit=rpart(formula=formula,data=data[d.ind,])
pfit=prune(fit,cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
p1=predict(pfit,data[t.ind,])
p=c(p,p1)}

for(i in 1:5){
t.ind=t2.ind[i,]
d.ind=k[-t.ind]
fit=rpart(formula=formula,data=data[d.ind,])
pfit=prune(fit,cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
p1=predict(pfit,data[t.ind,])
p=c(p,p1)}
pt=p
pt[p<0.5]=0
pt[p>=0.5]=1

tabl=table(data[b,1],pt)
print(tabl)

acc.d[j]=acc(tabl)
sn.d[j]=sn(tabl)
sp.d[j]=sp(tabl)
ppv.d[j]=ppv(tabl)
npv.d[j]=npv(tabl)}

acc.m=mean(na.exclude(acc.d))
sd.acc=sd(na.exclude(acc.d))
sn.m=mean(na.exclude(sn.d))
sd.sn=sd(na.exclude(sn.d))
sp.m=mean(na.exclude(sp.d))
sd.sp=sd(na.exclude(sp.d))
ppv.m=mean(na.exclude(ppv.d))
sd.ppv=sd(na.exclude(ppv.d))

npv.m=mean(na.exclude(npv.d))
sd.npv=sd(na.exclude(npv.d))
acc.m
sd.acc
sn.m
sd.sn
sp.m
sd.sp
ppv.m
sd.ppv
npv.m
sd.npv



