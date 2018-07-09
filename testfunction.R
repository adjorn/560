
yif=function(data){
a0=data[data[,3]==0,]
a1=data[data[,3]==1,]
a2=data[data[,3]==2,]
y0=sum(a0[,2]==1)
y1=sum(a1[,2]==1)
y2=sum(a2[,2]==1)
yi=c(y0,y1,y2)
  return(yi)}

wf=function(data){
tmax=108
n=nrow(data)
w=numeric(n)
w=(data[,1]/tmax)^3
w[data[,2]==1]=1
return(w)}


rif=function(data){
w=wf(data)
r0=sum(w[data[,3]==0])
r1=sum(w[data[,3]==1])
r2=sum(w[data[,3]==2])
ri=c(r0,r1,r2)
 return(ri)}

cf=function(data){
N=nrow(data)
ng=N/50
yh=numeric(N)
yh[data[,2]==1]=1
wh=wf(data)
Ni=50
ri=rif(data)
yi=yif(data)
y=sum(yi)
p=y/sum(ri)
vh=yh-p*wh
h=1:N
h0=h[data[,3]==0]
h1=h[data[,3]==1]
h2=h[data[,3]==2]
v0.m=sum(vh[h0])/Ni
v1.m=mean(vh[h1])
v2.m=mean(vh[h2])
c=sum(vh[h0]-v0.m)^2+sum(vh[h1]-v1.m)^2+sum(vh[h2]-v2.m)^2
c=c/(N-ng)
return(c)
}



bwp3.test=function(data){
Ni=50
ri=rif(data)
yi=yif(data)
pi=yi/ri
y=sum(yi)
p=y/sum(ri)
ai=ri^2/Ni
c=cf(data)
di=c(0,1,2)
x=sum(ai*pi*di)-(sum(ai*di))*(sum(ai*pi))/sum(ai)
v=c*(sum(ai*di^2)-(sum(ai*di))^2/sum(ai))
z=x/sqrt(v)
return(pnorm(z))}

