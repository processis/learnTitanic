# chapter 28

#28.1.1 Investigating the route to chaos

par(mfrow=c(2,2))
lambda <- 2
x <- numeric(40)
x[1] <- 0.6
for (t in 2 : 40) x[t] <- lambda * x[t-1] * (1- x[t-1])
plot(1:40,x,type="l",ylim=c(0,1),ylab="population",
     xlab="time",main="lambda = 2.0")

lambda <- 3.3
x <- numeric(40)
x[1] <- 0.6
for (t in 2 : 40) x[t] <- lambda * x[t-1] * (1- x[t-1])
plot(1:40,x,type="l",ylim=c(0,1),ylab="population",
     xlab="time",main="lambda = 3.3")

lambda <- 3.5
x <- numeric(40)
x[1] <- 0.6
for (t in 2 : 40) x[t] <- lambda * x[t-1] * (1- x[t-1])
plot(1:40,x,type="l",ylim=c(0,1),ylab="population",
     xlab="time",main="lambda = 3.5")

lambda <- 4
x <- numeric(40)
x[1] <- 0.6
for (t in 2 : 40) x[t] <- lambda * x[t-1] * (1- x[t-1])
plot(1:40,x,type="l",ylim=c(0,1),ylab="population",
     xlab="time",main="lambda = 4.0")


# 28.1.1 Investigating the route to chaos

numbers <- function (lambda) {
  x <- numeric(400)
  x[1] <- 0.6
  for (t in 2 : 400) x[t] <- lambda * x[t-1] * (1- x[t-1])
  x[381:400] }

par(mfrow=c(1,1))
plot(c(2,4),c(0,1),type="n",xlab="lambda",ylab="population")

for(lam in seq(2,4,0.01))
  points(rep(lam,20),sapply(lam,numbers),pch=16,cex=0.5,col="blue")

# 28.2 Temporal and spatial dynamics: A simulated random walk in two dimensions

sample(c(1,0,-1),1)

plot(0:100,0:100,type="n",xlab="",ylab="")
x <- y <- 50
points(50,50,pch=16,col="red",cex=1.5)

for (i in 1:10000) {
  xi <- sample(c(1,0,-1),1)
  yi <- sample(c(1,0,-1),1)
  lines(c(x,x+xi),c(y,y+yi),col="blue")
  x <- x+xi
  y <- y+yi
  if (x>100 | x<0 | y>100 | y<0) break
}

# 28.3 Spatial simulation models

m <- 0.15
e <- 0.1

s <- (1-e)
N <- matrix(rep(0,10000),nrow=100)
xs <- sample(1:100)
ys <- sample(1:100)
for (i in 1:100){
  N[xs[i],ys[i]] <- 1 }
image(1:100,1:100,N)

for (t in 1:1000){
  
  S <- matrix(runif(10000),nrow=100)
  N <- N*(S<s)
  
  im <- floor(sum( N*m))
  
  placed <- matrix(sample(c(rep(1,im) ,rep (0,10000-im))),nrow=100)
  N <- N+placed
  N <- apply(N,2,function(x) ifelse(x>1,1,x))
  
  image(1:100,1:100,N,add=TRUE)
  box(col="red")
}

sum(N)/length(N)

m <- 0.15
e <- 0.1
s <- (1-e)
N <- matrix(rep(0,10000),nrow=100)
xs <- sample(1:100)
ys <- sample(1:100)
for (i in 1:100){
  N[xs[i],ys[i]] <- 1 }
image(1:100,1:100,N)
for (t in 1:1000){
  S <- matrix(runif(10000),nrow=100)
  N <- N*(S<s)
  
  
  im <- floor(sum( N*m))
  placed <- matrix(sample(c(rep(1,im) ,rep (0,10000-im))),nrow=100)
  N <- N+placed
  N <- apply(N,2,function(x) ifelse(x>1,1,x))
  image(1:100,1:100,N,add=TRUE)
  box(col="red")
}

# 28.3.2 Coexistence resulting from spatially explicit (local) density dependence

plot(c(0,1),c(0,1),xaxt="n",yaxt="n",type="n",xlab="",ylab="")
abline("v"=c(1/3,2/3))
abline("h"=c(1/3,2/3))
xs <- c(.15,.5,.85,.15,.85,.15,.5,.85)
ys <- c(.85,.85,.85,.5,.5,.15,.15,.15)
for (i in 1:8) text(xs[i],ys[i],as.character(i))
text(.5,.5,"target cell")

margins <- function(N){
  edges <- matrix(rep(0,10404),nrow=102)
  edges[2:101,2:101] <- N
  edges[1,2:101] <- N[100,]
  edges[102,2:101] <- N[1,]
  edges[2:101,1] <- N[,100]
  edges[2:101,102] <- N[,1]
  edges[1,1] <- N[100,100]
  edges[102,102] <- N[1,1]
  edges[1,102] <- N[100,1]
  edges[102,1] <- N[1,100]
  edges}

nhood <- function(X,j,i) sum(X[(j-1):(j+1),(i-1):(i+1)]==1)

Ra <- 3
Rb <- 2.0
D <- 0.25
s <- (1-D)
T <- 6

N <- matrix(c(rep(1,5000),rep(2,5000)),nrow=100)
image(1:100,1:100,N)

for (t in 1:1000) {
  S <- 1*(matrix(runif(10000),nrow=100)<s)
  N <- N*S
  space <- 10000-sum(S)
  nt <- margins(N)
  tots <- matrix(rep(0,10000),nrow=100)
  for (a in 2:101) {
    for (b in 2:101) {
      tots[a-1,b-1] <- nhood(nt,a,b)
    }}
  
  seedsA <- sum(N==1)*Ra
  seedsB <- sum(N==2)*Rb
  all.seeds <- seedsA+seedsB
  fA <- seedsA/all.seeds
  fB <- 1-fA
  
  setA <- ceiling(10000*fA)
  placed <- matrix(sample(c(rep(1,setA) ,rep (2,10000-setA))),nrow=100)
  
  for (i in 1:100){
    for(j in 1:100){
      if (N[i,j] == 0 )
        if(placed[i,j]== 2) N[i,j] <- 2
        else
          if (tots[i,j]>=T) N[i,j] <- 2
          else N[i,j] <- 1
    }}
  
  image(1:100,1:100,N,add=TRUE)
  box(col="red")}

# 28.4 Pattern generation resulting from dynamic interactions

r <- 0.4
a <- 0.1
Hmr <- 0.1
Pmr <- 0.9

N <- matrix(rep(0,10000),nrow=100)
P <- matrix(rep(0,10000),nrow=100)

N[33,33] <- 200
P[33,33] <- 100
image(1:100,1:100,N)

host <- function(N,P) N*exp(r-a*P)
parasite <- function(N,P) N*(1-exp(-a*P))

host.edges <- function(N){
  Hedges <- matrix(rep(0,10404),nrow=102)
  Hedges[2:101,2:101] <- N
  Hedges[1,2:101] <- N[100,]
  Hedges[102,2:101] <- N[1,]
  Hedges[2:101,1] <- N[,100]
  Hedges[2:101,102] <- N[,1]
  Hedges[1,1] <- N[100,100]
  Hedges[102,102] <- N[1,1]
  Hedges[1,102] <- N[100,1]
  Hedges[102,1] <- N[1,100]
  Hedges}
parasite.edges <- function(P)
{
  Pedges <- matrix(rep(0,10404),nrow=102)
  Pedges[2:101,2:101] <- P
  Pedges[1,2:101] <- P[100,]
  Pedges[102,2:101] <- P[1,]
  Pedges[2:101,1] <- P[,100]
  Pedges[2:101,102] <- P[,1]
  Pedges[1,1] <- P[100,100]
  Pedges[102,102] <- P[1,1]
  Pedges[1,102] <- P[100,1]
  Pedges[102,1] <- P[1,100]
  Pedges}
  
  nhood <- function(X,j,i) sum(X[(j-1):(j+1),(i-1):(i+1)])
  
  h.migration <- function(Hedges){
    Hmigs <- matrix(rep(0,10000),nrow=100)
    for (a in 2:101) {
      for (b in 2:101) {
        Hmigs[a-1,b-1] <- nhood(Hedges,a,b)
      }}
    Hmigs}
  
  p.migration <- function(Pedges){
    Pmigs <- matrix(rep(0,10000),nrow=100)
    for (a in 2:101) {
      for (b in 2:101) {
        Pmigs[a-1,b-1] <- nhood(Pedges,a,b)
      }}
    Pmigs}
  
  for (t in 1:600){
    he <- host.edges(N)
    pe <- parasite.edges(P)
    Hmigs <- h.migration(he)
    Pmigs <- p.migration(pe)
    N <- N-Hmr*N+Hmr*Hmigs/9
    P <- P-Pmr*P+Pmr*Pmigs/9
    Ni <- host(N,P)
    P <- parasite(N,P)
    N <- Ni
    image(1:100,1:100,N,add=TRUE)
  }
  
