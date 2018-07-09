
##age adjusted scheme.
##for one group.
gdiv=function(d,di){

a=d[d[,3]==di,]
a=a[order(a[,1]),]
ds=d[order(d[,1]),]##sort the first column of data set.
low=j=0
k=matrix(numeric(50*2),ncol=2)##store group intervals.
for(i in 1:50){
up=a[i,1]
if(i==50){
   j=j+1
   k[j,1]=low
   k[j,2]=up}else{
  int1all=sum(low<ds[,1]&ds[,1]<=up)
  int2all=sum(low<ds[,1]&ds[,1]<=a[i+1,1])
  if(int1all==int2all){next}else{
    j=j+1
    k[j,1]=low
    k[j,2]=up
    low=up}
}
}

k=k[k[,2]!=0,]
nint=nrow(k)
s=deths=numeric(nint)
gi=NULL
arr.ind=1:150
for(i in 1:nint){


s[i]=sum(k[i,1]<a[,1]&a[,1]<=k[i,2])##sample size of interval i
deths[i]=sum(k[i,1]<a[a[,2]==1,1]&a[a[,2]==1,1]<=k[i,2])##numer of death in interval i
if(k[i,1]==0){lowi=1}else{
   lowi=max(which(ds[,1]==k[i,1],arr.ind=T))+1}
##the lower bound of interval i is the same as uper bound of interval i-1,which has been chosen.
##so the lowi =up[i-1]+1
##The interval (9,13] is the same as[10,13].For resampling ind 10 can be select,but 9 cant be.

upi=max(which(ds[,1]==k[i,2],arr.ind=T))
pool.ind=lowi:upi##index in whole ordered data set.

l=numeric(150)
l[lowi:upi]=1##get logical value from interval[lowi,upi]
lpool=arr.ind[l&ds[,2]==2]##index of alive samples of all in the interval i

g.ind=sample(pool.ind,size=s[i],replace=T)
l=numeric(150)
l[g.ind]=1
gd.ind=arr.ind[l&ds[,2]==1]##index of deaths.
nd=length(gd.ind)-deths[i]##number of deaths over the max.
if(nd>0){
   ls=sample(lpool,size=nd,replace=T)##resample alive sample in alive sample pool
   gdr.ind=sample(gd.ind,size=nd,replace=F)##choose the delete index of deaths..
   for(f in 1:nd){g.ind[g.ind[f]==gdr.ind[f]]=ls[f]}
   gi=rbind(gi,ds[g.ind,])
   }else{gi=rbind(gi,ds[g.ind,])}
}
gi[,3]=di
return(gi)
}

##one time bootstrap function for age ajusted skeme.
adjf=function(data){
   g0=gdiv(data,0)
   g1=gdiv(data,1)
   g2=gdiv(data,2)
   return(rbind(g0,g1,g2))}

##one time bootstrap function for original bootstrap method
orig=function(data){
   nr=nrow(data)
   inds=1:nr
   
   ind0=sample(inds,size=50,replace=T)
   g0=data[ind0,]
   g0[,3]=0
   ind1=sample(inds,size=50,replace=T)
   g1=data[ind1,]
   g1[,3]=1
   ind2=sample(inds,size=50,replace=T)
   g2=data[ind2,]
   g2[,3]=2
   return(rbind(g0,g1,g2))}




    
   
  

   
