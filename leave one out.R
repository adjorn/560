n=nrow(data)
p=NULL
for(i in 1:n){
fit=rpart(formula=formula,data=data[d1,])
pfit=prune(fit,cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
p1=predict(pfit,data[i,])
p=c(p,p1)}
pt=p



pt[p<0.5]=0
pt[p>=0.5]=1
t=table(data[,1],pt)

acc.l=acc(t)
sn.l=sn(t)
sp.l=sp(t)
ppv.l=ppv(t)
npv.l=npv(t)
acc.l
sn.l
sp.l
ppv.l
npv.l


library(Daim)
m=roc(p,data[,1],1)
summary(m)
plot(m)
roc.area(m)


