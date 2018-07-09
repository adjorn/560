
##T1 data generating funcion.


tr.dose=function(theta,x){
##x is tumor rate from control group, theta is hazard rate, delta1 is fix to both group. theta reflects experiment interest.
   delta1=-log(1-x)
   return(1-exp(theta*log(1-x)))}

## tumor onset function 
## Generate data sets with Weibull tumor onset distribution with shape parameter delta2 equal to 3 for each dose group in each combination.
t1f=function(theta,x){
##theta=1,x is tumorate for control group.if theta=other number,x is tumor rate for dose group with theta.
## set the sacrificed date to be 108. Right censor is applied. 
tmax=108
U=runif(50)
delta1=-log(1-x)/theta ##here theta is not to be constant 1.
## set shape parameter delta2 equal to 3.
return(tmax*(-log(1-U)/(theta*delta1))^(1/3))}

##tumor rate
trf=function(t1){
tmax=108
mean(t1<tmax)}

## T3 data generating funcion
## lamda3 is positive, the function of t, saying w(t),is an increasing function.
########################################################################################################################
## CRSR stands for competing risks survival rate.Competing risks are said to be present when a patient is at risk of  ##
## more than one mutually exclusive event, such as death from different causes, and the occurrence of one of these    ##
## will prevent any other event from ever happening                                                                   ##  
########################################################################################################################


phinef=function(crsr.d,crsr.c){log(crsr.d)/log(crsr.c)}
## c stands for control group. d stands for dose group.
## T3 stands for time to death due to competing risks.
## T3 generate function.      
t3f=function(crsr.c,phine){
##If phine=phine.c, generate control group t3;else if phine=phine.d, generate dose group t3.
lamda1=10^(-4)
lamda2=10^(-16)
tmax=108
lamda3f=function(crsr.c){log(-(log(crsr.c)+lamda1*tmax)/lamda2)/log(tmax)}
w3=function(t,crsr.c){return(lamda1*t+lamda2*t^lamda3f(crsr.c))}
U=runif(50)
reps=numeric(50)
for(i in 1:50){
   reps[i]=uniroot(function(t){w3(t,crsr.c)+log(1-U[i])/phine},c(0,10*tmax))$root}
return(reps)}

##death rate function for T3
drf=function(t){
   tmax=108
   mean(t<tmax)}


## T2 generating function,the same as T3 but phine different.
## T2 is time to death due to tumor of interest.
##phine2 can be found from table
t2f=function(crsr.c,phine2){
lamda1=10^(-4)
lamda2=10^(-16)
tmax=108
lamda3f=function(crsr.c){log(-(log(crsr.c)+lamda1*tmax)/lamda2)/log(tmax)}
w3=function(t,crsr.c){lamda1*t+lamda2*t^lamda3f(crsr.c)}
U=runif(50)
reps=numeric(50)
for(i in 1:50){
   reps[i]=uniroot(function(t){w3(t,crsr.c)+log(1-U[i])/phine2},c(0,10*tmax))$root}
return(reps)}

lrf=function(t1,t2){##lethal rate funtion.
tmax=108
td=t1+t2
if(sum(t1<tmax)==0){return(0)}else{return(sum(td<tmax)/sum(t1<tmax))}}





testf=function(x,theta,crsr){
B=2000
theta.0=theta[1]
theta.1=theta[2]
theta.2=theta[3]
crsr.0=crsr[1]
crsr.1=crsr[2]
crsr.2=crsr[3]

if(x==0.1){
   phine2.0=phine2.1=phine2.2=175}else if(x==0.2){
     phine2.0=phine2.1=phine2.2=170}else if(x==0.4){
       phine2.0=phine2.1=phine2.2=152}


##for control group.

phine.0=1## phine for T3. phine.0=1

tr1=dr1=lr1=numeric(B)
for(i in 1:B){
   t1.0=t1f(theta.0,x)##theta =1 for control group,0.1 is tumor rate for control group.
   tr1[i]=trf(t1.0)
   t3.0=t3f(crsr.0,phine.0)
   dr1[i]=drf(t3.0)
   t2.0=t2f(crsr.0,phine2.0)
   lr1[i]=lrf(t1.0,t2.0)
   }
tr0.m=mean(tr1)
tr0.sd=sd(tr1)
dr0.m=mean(dr1)
dr0.sd=sd(dr1)
lr0.m=mean(lr1)
lr0.sd=sd(lr1)
tr0.m
tr0.sd
dr0.m
dr0.sd
lr0.m
lr0.sd

m0=matrix(c(c(tr0.m,dr0.m,lr0.m,0),c(tr0.sd,dr0.sd,lr0.sd,0)),nrow=2,byrow=T,
  dimnames=list(c("m","sd"),c("tr","dr","lr","group")))



##for dose group 1

y=tr.dose(theta.1,x)
y
phine.1=phinef(crsr.1,crsr.0)
tr1=dr1=lr1=numeric(B)
for(i in 1:B){
   t1.1=t1f(theta.1,y)##y is turmor rate for dose group.
   tr1[i]=trf(t1.1)
   t3.1=t3f(crsr.0,phine.1)
   dr1[i]=drf(t3.1)
   t2.1=t2f(crsr.0,phine2.1)
   lr1[i]=lrf(t1.1,t2.1)
   }
tr1.m=mean(tr1)
tr1.sd=sd(tr1)
dr1.m=mean(dr1)
dr1.sd=sd(dr1)
lr1.m=mean(lr1)
lr1.sd=sd(lr1)
tr1.m
tr1.sd
dr1.m
dr1.sd
lr1.m
lr1.sd
m1=matrix(c(c(tr1.m,dr1.m,lr1.m,1),c(tr1.sd,dr1.sd,lr1.sd,1)),nrow=2,byrow=T,
  dimnames=list(c("m","sd"),c("tr","dr","lr","group")))

##for dose group 2

z=tr.dose(theta.2,x)
z
phine.2=phinef(crsr.2,crsr.0)
tr1=dr1=lr1=numeric(B)
for(i in 1:B){
   t1.2=t1f(theta.2,z)##y is turmor rate for dose group.
   tr1[i]=trf(t1.2)
   t3.2=t3f(crsr.0,phine.2)
   dr1[i]=drf(t3.2)
   t2.2=t2f(crsr.0,phine2.2)
   lr1[i]=lrf(t1.2,t2.2)
   }
tr2.m=mean(tr1)
tr2.sd=sd(tr1)
dr2.m=mean(dr1)
dr2.sd=sd(dr1)
lr2.m=mean(lr1)
lr2.sd=sd(lr1)
tr2.m
tr2.sd
dr2.m
dr2.sd
lr2.m
lr2.sd

m2=matrix(c(c(tr2.m,dr2.m,lr2.m,2),c(tr2.sd,dr2.sd,lr2.sd,2)),nrow=2,byrow=T,
  dimnames=list(c("m","sd"),c("tr","dr","lr","group")))


m=rbind(m0,m1,m2)
return(m)
}

theta=c(1,1.5,2)
crsr=c(0.6,0.6,0.6)
x=0.1
test1=testf(x=x,theta=theta,crsr=crsr)
test1
x=0.2
test2=testf(x=x,theta=theta,crsr=crsr)
test2
x=0.4
test3=testf(x=x,theta=theta,crsr=crsr)
test3

crsr=c(0.6,0.5,0.3)
x=0.1
test4=testf(x=x,theta=theta,crsr=crsr)
test4
x=0.2
test5=testf(x=x,theta=theta,crsr=crsr)
test5
x=0.4
test6=testf(x=x,theta=theta,crsr=crsr)
test6



