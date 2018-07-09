B=500
n=nrow(data)
k=1:n
for(j in 1:B){
d1=sample(k,size=n,replace=T)
dt=setdiff(k,d1)
fit=rpart(formula=formula,data=data[d1,])
pfit=prune(fit,cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
p=predict(pfit,data[dt,])
pt=p
pt[p<0.5]=0
pt[p>=0.5]=1
t=table(data[dt,1],pt)
acc.d[j]=acc(t)
sn.d[j]=sn(t)
sp.d[j]=sp(t)
ppv.d[j]=ppv(t)
npv.d[j]=npv(t)}


npv.macc.m=mean(na.exclude(acc.d))
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
