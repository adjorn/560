##data generating function

datf=function(x,theta,crsr){
##theta and crsr are vecters.x is tumor rate of control group.
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

tmax=108
N=150
dat.0=dat.1=dat.2=matrix(numeric(50*3),ncol=3)

phine.0=1
t1.0=t1f(theta.0,x)
t3.0=t3f(crsr.0,phine.0)
t2.0=t2f(crsr.0,phine2.0)
td.0=t1.0+t2.0
for(i in 1:50){
if(t1.0[i]<=min(t3.0[i],tmax)){
  dat.0[i,1]=min(td.0[i],t3.0[i],tmax)
  dat.0[i,2]=1
  dat.0[i,3]=0}else{
     dat.0[i,1]=min(t3.0[i],tmax)
     dat.0[i,2]=2
     dat.0[i,3]=0}
}

z1=tr.dose(theta.1,x)

phine.1=phinef(crsr.1,crsr.0)
t1.1=t1f(theta.1,z1)
t3.1=t3f(crsr.1,phine.1)
t2.1=t2f(crsr.1,phine2.1)
td.1=t1.1+t2.1
for(i in 1:50){
if(t1.1[i]<=min(t3.1[i],tmax)){
  dat.1[i,1]=min(td.1[i],t3.1[i],tmax)
  dat.1[i,2]=1
  dat.1[i,3]=1}else{
     dat.1[i,1]=min(t3.0[i],tmax)
     dat.1[i,2]=2
     dat.1[i,3]=1}
}

z2=tr.dose(theta.2,x)

phine.2=phinef(crsr.2,crsr.0)
t1.2=t1f(theta.2,z2)
t3.2=t3f(crsr.2,phine.2)
t2.2=t2f(crsr.2,phine2.2)
td.2=t1.2+t2.2
for(i in 1:50){
if(t1.2[i]<=min(t3.2[i],tmax)){
  dat.2[i,1]=min(td.2[i],t3.2[i],tmax)
  dat.2[i,2]=1
  dat.2[i,3]=2}else{
     dat.2[i,1]=min(t3.2[i],tmax)
     dat.2[i,2]=2
     dat.2[i,3]=2}
}
dat=rbind(dat.0,dat.1,dat.2)
return(dat)}