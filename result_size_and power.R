##generate 1000 samples and perform 5000 bootstrap tests for two skems.
R=1000
B=5000

##compare the size of two tests.generate three groups have the same the same theta(1).that means all control groups.
siz=function(x,theta,crsr){
rej0=rej1=numeric(R)
for(f in 1:R){
dat=datf(x,theta,crsr)##generate one data set
T0=T1=numeric(B)
for(v in 1:B){
dat0=orig(dat)## one time no ajust bootstrap sample.
dat1=adjf(dat)##one time age ajusted bootstrap sample.
T0[v]=bwp3.test(dat0)
T1[v]=bwp3.test(dat1)
}
cr0=quantile(na.exclude(T0),0.95)
cr1=quantile(na.exclude(T1),0.95)
T=bwp3.test(dat)
rej0[f]=(T>=cr0)
rej1[f]=(T>=cr1)}

size0=mean(rej0)
size1=mean(rej1)
size=matrix(c(size0,size1),byrow=T,nrow=1,dimnames=list("size",c("orig method","adj method")))
return(size)}


##
powr=function(x,theta,crsr){
rej0=rej1=numeric(R)
for(f in 1:R){
dat=datf(x,theta,crsr)##generate one data set
T0=T1=numeric(B)
for(v in 1:B){
dat0=orig(dat)## one time no ajust bootstrap sample.
dat1=adjf(dat)##one time age ajusted bootstrap sample.
T0[v]=bwp3.test(dat0)
T1[v]=bwp3.test(dat1)
}
cr0=quantile(na.exclude(T0),0.95)
cr1=quantile(na.exclude(T1),0.95)
T=bwp3.test(dat)
rej0[f]=(T>=cr0)
rej1[f]=(T>=cr1)}

powr0=mean(rej0)
powr1=mean(rej1)
powr=matrix(c(powr0,powr1),byrow=T,nrow=1,dimnames=list("powr",c("orig method","adj method")))
return(powr)}









##set 3 groups have the same tumor rate to get size of test.
##I ran the program in two computers with R=400,B=400 and R=1000,B=5000 respectively.However I could only get the R=400,B=400 for size of test.
R=400
B=400
theta=c(1,1,1)
crsr=c(0.6,0.6,0.6)
x=0.1
siz(x,theta,crsr)

crsr=c(0.6,0.5,0.3)
siz(x,theta,crsr)





##set 2 groups have the same tumor rate and the other has different tumor rate to get the power of test
##It cost too much of time to run the program to get the size of test, so I have to change B and R
R=200
B=400
theta=c(1,1,2)
crsr=c(0.6,0.6,0.6)
x=0.1
powr(x,theta,crsr)

crsr=c(0.6,0.5,0.3)
powr(x,theta,crsr)


theta=c(1,1.5,1)
crsr=c(0.6,0.6,0.6)
powr(x,theta,crsr)

crsr=c(0.6,0.5,0.3)
powr(x,theta,crsr)

