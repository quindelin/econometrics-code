# I have not included code for storing the computation time and the seed number

# You must change the filename for the data that you store to include your netID

library(bindata)

seed=10011998 #your seed number must be different
set.seed(seed)

ormat<-function(pX,pXY){
  pden<-denom<-mat<-diag(1,length(pX)) #1s (ones) along the diagonal
  for(rows in 1:length(pX) ){
    for(cols in 1:length(pX) ){
      if(rows!=cols){
        pden[rows,cols]=(pX[cols]-pXY[paste(rows,cols,sep=",")] )*( pX[rows]-pXY[paste(rows,cols,sep=",")] ) 
        denom[rows,cols]=ifelse( abs(pden[rows,cols])<1e-8, pden[rows,cols]+1e-4, pden[rows,cols])
        mat[rows,cols]<-pXY[paste(rows,cols,sep=",")]*( 1-pX[cols]-pX[rows]+pXY[paste(rows,cols,sep=",")] )/denom[rows,cols]
      }
    }
  }
  return(mat)
}

##################
#BEGIN PARAMETERS#
##################
#True model has:
N<-500 #samples for each stratum
TIME<-10 #time points (pair of data)
PROD<-10 #covariates 
SIM<-10
param<-4 #This represents the number of parameters in the function for the estimated eigenvalues (not the true eigenvalues)
TT<-matrix(0,nrow=TIME,ncol=param)
for (pars in 1:param){
  TT[,pars]<-( (1:TIME)/TIME )^(param-pars)
}

nm<-NULL
for(i in 1:PROD){
  for(j in 1:PROD){
    if(i!=j) {
      nm<-c(nm,c(paste(i,j,sep=","))) }
  }
}

#time-varying eigenvalues
alp1<-alp2<-NULL
for (t in 1:TIME){
  alp1[t]<-(0.2*(t/10)^2 - 0.2*(t/10) + 0.19)^(-1)#(0.2*(t/10)^2 - 0.2*(t/10) + 0.19)^(-1)
  alp2[t]<-(-0.5*(t/10)^2 + 0.5*(t/10) + 0.30)^(-1)#(-0.5*(t/10)^2 + 0.5*(t/10) + 0.30)^(-1)
}

alp3<-(rep(0.91,TIME))^(-1)
alp4<-(rep(0.92,TIME))^(-1)
alp5<-(rep(0.93,TIME))^(-1)
alp6<-(rep(0.94,TIME))^(-1)
alp7<-(rep(0.95,TIME))^(-1)
alp8<-(rep(0.96,TIME))^(-1)
alp9<-(rep(0.97,TIME))^(-1)
alp10<-(rep(0.98,TIME))^(-1)
alps<-cbind(alp1,alp2,alp3,alp4,alp5,
            alp6,alp7,alp8,alp9,alp10)
cumvar<-(colMeans(alps)[1]+colMeans(alps)[2])/sum(colMeans(alps))
cumvar

r12<-rep(0.7,10)#no change
r13<-rep(0.7,10)#no change
r14<-rep(0.7,10)#no change
r15<-rep(0.7,10)#no change
r16<-round(seq(0.1,0.7,length=10),2)
r17<-round(seq(0.1,0.7,length=10),2)
r18<-rep(0.1,10)#no change
r19<-rep(0.1,10)#no change
r110<-rep(0.1,10)#no change
r23<-rep(0.7,10)#no change
r24<-rep(0.7,10)#no change
r25<-rep(0.7,10)#no change
r26<-round(seq(0.1,0.7,length=10),2)
r27<-round(seq(0.1,0.7,length=10),2)
r28<-rep(0.1,10)#no change
r29<-rep(0.1,10)#no change
r210<-rep(0.1,10)#no change
r34<-rep(0.7,10)#no change
r35<-rep(0.7,10)#no change
r36<-round(seq(0.1,0.7,length=10),2)
r37<-round(seq(0.1,0.7,length=10),2)
r38<-rep(0.1,10)#no change
r39<-rep(0.1,10)#no change
r310<-rep(0.1,10)#no change
r45<-rep(0.7,10)#no change
r46<-round(seq(0.1,0.7,length=10),2)
r47<-round(seq(0.1,0.7,length=10),2)
r48<-rep(0.1,10)#no change
r49<-rep(0.1,10)#no change
r410<-rep(0.1,10)#no change
r56<-round(seq(0.1,0.7,length=10),2)
r57<-round(seq(0.1,0.7,length=10),2)
r58<-rep(0.1,10)#no change
r59<-rep(0.1,10)#no change
r510<-rep(0.1,10)#no change
r67<-rep(0.8,10)#no change
r68<-round(seq(0.7,0.1,length=10),2)
r69<-round(seq(0.7,0.1,length=10),2)
r610<-round(seq(0.7,0.1,length=10),2)
r78<-round(seq(0.7,0.1,length=10),2)
r79<-round(seq(0.7,0.1,length=10),2)
r710<-round(seq(0.7,0.1,length=10),2)
r89<-rep(0.8,10)#no change
r810<-rep(0.8,10)#no change
r910<-rep(0.8,10)#no change

#Begin with 10x10 true correlation matrix which will be used to generate y
VV<-lapply(1:10,function(a){matrix(c(
  1,r12[a],r13[a],r14[a],r15[a],r16[a],r17[a],r18[a],r19[a],r110[a],
  r12[a],1,r23[a],r24[a],r25[a],r26[a],r27[a],r28[a],r29[a],r210[a],
  r13[a],r23[a],1,r34[a],r35[a],r36[a],r37[a],r38[a],r39[a],r310[a],
  r14[a],r24[a],r34[a],1,r45[a],r46[a],r47[a],r48[a],r49[a],r410[a],
  r15[a],r25[a],r35[a],r45[a],1,r56[a],r57[a],r58[a],r59[a],r510[a],
  r16[a],r26[a],r36[a],r46[a],r56[a],1,r67[a],r68[a],r69[a],r610[a],
  r17[a],r27[a],r37[a],r47[a],r57[a],r67[a],1,r78[a],r79[a],r710[a],
  r18[a],r28[a],r38[a],r48[a],r58[a],r68[a],r78[a],1,r89[a],r810[a],
  r19[a],r29[a],r39[a],r49[a],r59[a],r69[a],r79[a],r89[a],1,r910[a],
  r110[a],r210[a],r310[a],r410[a],r510[a],r610[a],r710[a],r810[a],r910[a],1
),ncol=PROD,nrow=PROD)})

ev<-lapply(1:TIME,function(a){eigen(VV[[a]])$vec} ) #not time-varying bc we fix this covariance type of matrix

O<-lapply(1:TIME,function(a){alp1[a]*ev[[a]][,1]%*%t(ev[[a]][,1])+alp2[a]*ev[[a]][,2]%*%t(ev[[a]][,2])+alp3[a]*ev[[a]][,3]%*%t(ev[[a]][,3])+alp4[a]*ev[[a]][,4]%*%t(ev[[a]][,4])+alp5[a]*ev[[a]][,5]%*%t(ev[[a]][,5])+alp6[a]*ev[[a]][,6]%*%t(ev[[a]][,6])+alp7[a]*ev[[a]][,7]%*%t(ev[[a]][,7])+alp8[a]*ev[[a]][,8]%*%t(ev[[a]][,8])+alp9[a]*ev[[a]][,9]%*%t(ev[[a]][,9])+alp10[a]*ev[[a]][,10]%*%t(ev[[a]][,10])})

COR<-lapply(1:TIME,function(a){cov2cor(O[[a]])})
lapply(COR,function(x){range(x[x<1])})

#-----------------------------#
#Random effects initial values
#-----------------------------#
rr=array(0,dim=c(N,PROD,SIM))
for(sims in 1:SIM){
  for(prod in 1:PROD){
    rr[,prod,sims]=rnorm(N,0,0.10)
  }
}
mprr=exp(rr)/(1+exp(rr))
colMeans(mprr);range(mprr)

x<-array(0,dim=(c(N,PROD,TIME,SIM)))

#####################################
#SIMULATION START & ESTIMATION START#
#####################################

#---------------------#
#Data Generation Start#
#---------------------#
timeStart=Sys.time()

library(doParallel)
library(foreach)
c1 <- makeCluster(1)
registerDoParallel(c1)

for(sim in 1:SIM) {
  
  for(t in 1:TIME){
    
    foreach(n = icount(N)) %dopar% {
      library(bindata)
      x[n,,t,sim] <- rmvbin(1, margprob = mprr[n,,sim], bincorr = COR[[t]])
    } #end for n
    
  } #end for t
  
} #end for sim
timeEnd=Sys.time()

#Stack the data into N by PROD matrix (collapse over TIME and SIM)
##################################################################
xx1=xx2=xx3=xx4=xx5=xx6=xx7=xx8=xx9=xx10=NULL #xx is [N,PROD,TIME,SIM]
for(t in 1:TIME){
  xx1=cbind(xx1,as.vector(x[,,t,1]))
  xx2=cbind(xx2,as.vector(x[,,t,2]))
  xx3=cbind(xx3,as.vector(x[,,t,3]))  
  xx4=cbind(xx4,as.vector(x[,,t,4]))
  xx5=cbind(xx5,as.vector(x[,,t,5]))
  xx6=cbind(xx6,as.vector(x[,,t,6]))
  xx7=cbind(xx7,as.vector(x[,,t,7]))
  xx8=cbind(xx8,as.vector(x[,,t,8]))
  xx9=cbind(xx9,as.vector(x[,,t,9]))
  xx10=cbind(xx10,as.vector(x[,,t,10]))
}

X=rbind(xx1,xx2,xx3,xx4,xx5,xx6,xx7,xx8,xx9,xx10)

#EXPORTING RESULTS#
###################

write(t(X),file=paste("qpd2_seed",seed),ncolumns=TIME,sep=" ")


#I have not included code for storing the computation time and the seed number