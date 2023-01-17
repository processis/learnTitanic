#ch5

BernBeta = function(priorShape,dataVec,credMass=0.95,saveGraph=F)
{
  if(length(priorShape)!=2){
    stop("priorShape must have two components.")
  }
  if(any(priorShape<=0)){
    stop("priorShape components must be positive.")
  }
  
  if(length(dataVec!=1&dataVec!=0)){
    stop("dataVec must be a vector of isand 0s.")
  }
  if(any(credMass<=0|credMass>=1.0)){
    stop("credMass must be between 0 and 1.")
  }
  
  
  a=priorShape[1]
  b=priorShape[1]
  
  z=sum(dataVec==1)
  N=length(dataVec)
  
  
  postShape=c(a+z,b+N-z)
  
  pData=beta(z+a,N-z+b)/beta(a,b)
  
  source("HDIofICDF.R")
  hpdLim=HDIOfICDF(qbeta,shape1=postShape[1],shape2=postShape[2])
  
  
  binwidth=0.005
  Theta=seq(from=binwidth/2,to=1-(binwidth/2),by=binwidth)
  
  pTheta=dbeta(Theta,a,b)
  pDataGivenTheta = Theta^z*(1-Theta)^(N-z)
  
  pDataGivenTheta = dbeta(Theta,a+z,b+N-z)
  
  pThetaGivenTheta = dbeta(Theta,a+z,b+N-z)
  
  
  window(7,10)
  layout(matrix(c(1,2,3),nrow=3,ncol=1,byrow=FALSE))
  par(mar=c(3,3,1,0),mgp=c(2,1,0),mai=c(0.5,0.5,0.3,0.1))
  maxY=max(c(pTheta,pThetaGivenData))
  
  plot(Theta,pTheta,type="1",lwd=3,
       xlim=c(0,1),ylim=c(0,maxY),cex.axis=1.2,
       xlab=bquote(theta),ylab=bquote(p(theta)),cex.lab=1.5,
       main="Prior",cex.main=1.5)
  if(a>b){textx=0;textadj=c(0,1)}
  else {textx=1;textadj=c(1,1)}
  
  text(text,1.0*max(pThetaGivenData),
       bquote("beta("*theta*"|"*.(a)*","*.(b)*")"),
       cex=2.0,adj=textadj)
  
  plot(theta,pThetaGivenData,type="1",lwd=3,
       xlim=c(0,1),ylim=c(0,maxY),cex.axis=1.2,
       xlab=bquote(theta),ylab=bquote("p("+theta+"jd3"),
       cex.lab=1.5,main="Posterior",cexmain=1.5)
  if(a+z>b+N-z){textx=0:textadj=c(0,1)}
  else{textx=1;textadj=c(1,1)}
  text(textx,1.00+max(pThetaGivenData),cex=2.0,
       bquote("beta("+theta+"|"+(a+z)+","+(b+N-z)+")"),
       adj=textadj)
  
  plot(theta,pThetaGivenData,type="1",lwd=3,
       xlim=c(0,1),ylim=c(0,maxY),cex.axis=1.2,
       xlab=bquote(theta),ylab=bquote("p("+theta+"|D"),
       cex.lab=1.5,main="Posterior",cexmain=1.5)
  
  if(a+z>b+N-z){textx=0;textadj=c(0,1)}
  else{textx=1;textadj=c(1,1)}
  
  text(textx,0.75*max(pTtetaGivenData),cex=2.0,
       bquote("p(D)="*.(signif(pData,3))),adj=textadj)
  
  hpdHt=mean(c(dbeta(hptlim[1],a+z,b+N-z),dbeta(hpdLim[2],a+z,b+N-z)))
  lines(c(hpdLim[1],hpdLim[1]),c(-0.5,hpdHp),type="1",lty=2,lwd=1.5)
  lines(c(hpdLim[2],hpdLim[2]),c(-0.5,hpdHp),type="1",lty=2,lwd=1.5)
  lines(hpdLim,c(hpdHt,hpdHt),type="1",led=2)
  
  text(mean(hpdLim),hpdHt,bquote(.(100+credMass)*"%HDI"),adj=c(0.5,-1.0),cex=2.0)
  text(hpdLim[1],hpdHt,bquote(.(round(hpdLim[1],3))),adj=c(-1.0,-1.0),cex=1.2)
  text(hpdLim[2],hpdHt,bquote(.(round(hpdLim[2],3))),adj=c(-1.0,-1.0),cex=1.2)
  
  if(saveGraph){
    filename=paste("BernBeta_",a,"_",b,"_",z,"_",N,".eps",sep="")
    dev.copy2eps(file=filename)
    
  }
  return(postShape)
  s#top("priorShape component must be positive.")
}




priorA=100
priorB=1
actualDataZ=8
actualDataN=12
postA=priorA+actualDataZ
postB=priorB+actualDataN-actualDataZ

simSampleSize=actualDataN
nSimSamples=10000

simSampleZrecord=vector(length=nSimSamples)

for(sampleIDx in 1:nSimSamples){
  sampleTheta = rbeta(1,postA,postB)
  sampleData=sample(x=c(0,1),prob=c(1-sampleTheta,sampleTheta),
                    size=simSampleSize,replace=TRUE)
  simSampleZrecord[sampleIDx]=sum(sampleData)
  
}

hist(simSampleZrecord)



#ch8

model{
  for (i in 1:N1){y1[i]^dbern(theta1)}
  for (i in 1:N2){y2[i]^dbern(theta2)}
  
  theta1^dbeta(3,3)
  theta2^dbeta(3,3)
  
}

datalist = list(
  N1=7,
  y1=c(1,1,1,1,1,0,0),
  N2=7,
  y2=c(1,1,0,0,0,0,0)
)
modelData(bugsData(datalist))

datalist = list(
  N1=7,
  #y1=c(1,1,1,1,1,0,0),
  N2=7 ,
 #y2=c(1,1,0,0,0,0,0)
)

thetaDiff = theta1Sample = theta2Sample
source("plotPost.R")
window(7,4)
plotPost(thetaDiff , xlab=expression(theta[1]-theta[2]),compVal=0.0,
         breaks=30)


#8.8.1
nInt = 501
theta1=seq(from((1/nINt)/2),to=(1-((1/nInt)/2)),by=(1/nInt))
theta2=theta1

priorName=c("Beta","Ripples","Null","Alt")[1]
if(priorName=="Beta"){
  a1=3;b1=3;a2=3;b2=3;
  prior1=dbeta(theta1,a1,b1)
  prior2=dbeta(theta2,a2,b2)
  prior=outer(prior1,prior2)
  prior=prior/sum(prior)
}

if(priorName=="Ripples"){
  rippleAtPoint = function(theta1,theta2){
    m1=0;m2=1;k=0.75*pi
    sin((k*(theta1-m1))^2+(k*(theta2-m2))^2)^2
  }
  prior=outer(theta1,theta2,rippleAtPoint)
  prior=prior/sum(prior)
}

if(priorName=="Null"){
  prior=diag(1,nrow=length(theta1),ncol=length(theta1))
  prior=prior/sum(prior)
}

if(priorName=="Alt"){
  prior=matrix(1,nrow=length(theta1),ncol=length(theta2))
  prior=prior/sum(prior)
}

z1=5;N1=7;z2=2;N2=7
likeAtPoint=function(t1,t2){
  p=t1^z1*(1-t1)^(N1-z1)*t2^z2*(1-t2)^(N2-z2)
  return(p)
}
likelihood=outer(theta1,theta2,likeAtPoint)

pData=sum(prior*likelihood)
posterior=(prior*likelihood)/pData


plotFIleName=paste("BernTwoGrid",priorName,".eps",sep="")
credib=.95

rotate=(-25)
tile=25
parallelness=5.0
shadeval=0.05
perspcex=0.7
ncontour=9
zmax=max(c(max(posterior),max(prior)))


nteeth1=length(theta1)
thindex1=seq(1,nteeth1,by=round(nteeth1/30))
thindex1=c(thindex1,nteeth1)
thindex2=thindex1

window(7,10)
layout(matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=TRUE))
par(mar=c(3,3,1,0))
par(mgp=c(2,0,1))
par(mai=c(0.4,0.4,0.2,0.05))
par(pty="s")


persp(thetal[thindex1],theta2[thindex2],prior[thindex1,thindex2],
      xlab="theta1",ylab = "theta2",main = "Prior",cex=perspcex,lwd=0.1,
      xlim=c(0,1),ylim=c(0,1),zlim=c(0,zmax),zlab="p(ti,t2)",
      theta=rotate,phi=tilt,d=parallelness,shade=shadeval)
contour(theta1[thindex1],theta2[thindex2],prior[thindex1,thindex2],
        main=bquote(""),levels=signif(seq(0,zmax,length=ncontours),3),
        drawlabels=FALSE,xlab=bquote(theta[1],ylab=bquote(theta[2])))

persp(theta1[thindex1],theta2[thindex2],likelihood[thindex1,thindex2],
      xlab="theta1",ylab="theta2",main="Likelihood",ld=0.1,
      xlim=c(0,1),ylin=c(0,1),zlab="p(D|ti,t2)",cex=perspcex,
      theta=rotate,phi=tilt,d=parallelness,shade=shadeval)

contour(theta1[thindex1],theta2[thindex2],likelihood[thindex1,thindex2],
        main=bquote(""),nlevels=(ncontours-1),
        drawlabels=FALSE,xlab=bquote(theta[1],ylab=bquote(theta[2])))


maxlike=which(likelihood==max(likelihood),arr.ind = TRUE)
if(theta1[maxlike[1]]>0.5){textxpos=0;xadj=0
}else{textxpos=1;xadj=1}

if(theta2[maxlike[2]]>0.5){textxpos=0;xadj=0
}else{textxpos=1;xadj=1}

text(textxpos,textypos,ces=1.5,
     bquote("z1="*.(z1)*".N1=".(N1)*".z2="*.(z2)*".N2="*.(N2))
     adj=c(xadj,yadj))

persp(theta1[thindex1],theta2[thindex2],posterior[thindex1,thindex2],
      xlab="theta1",ylab="theta2",main = "Posteior",cex=perspcex,lwd=0.1,
      xlim=c(0,1),ylim=c(0,1),zlim=c(0,zmax),zlab="p(ti,t2|D)",
      theta=rotate,phi=tilt,d=parallelness,shade=shadeval)

contour(theta1[thindex1],theta2[thindex2],posterior[thindex1,thindex2],
        main=bquote(""),levels=signif(seq(0,zmax,length=ncontours),3),
        drawlabels=FALSE,xlab=bquote(theta[1],ylab=bquote(theta[2])))

maxpost=which(posterior==max(posterior),arr.ind=TRUE)

if(theta1[maxpost[1]]>0.5){textxpos=0;xadj=0
}else{textxpos=1;xadj=1}

if(theta2[maxpost[2]]>0.5){textxpos=0;xadj=0
}else{textxpos=1;xadj=1}

text(textxpos,textypos,ces=1.5,
     bquote("p(D)"*.(signif(pData,3))),
     adj=c(xadj,yadj))

source("HDIofGrid.R")
HDIheight=HDIofGrid(posterior)Sheight
par(new=TRUE)
contour(theta1[thindex1],theta2[thindex2],posterior[thindex1,thindex2],
        main=bquote(.100*credib)"% HD region"),
        levels=signif(HDIheight,3),lwd=3,
        drawlabels=FALSE,xlab=bquote(theta[1],ylab=bquote(theta[2]))

wantSaveGraph=T
if(wantSaveGraph){dev.copy2eps(file=plotFileName)}
