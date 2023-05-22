#8.2.1

myData = read.csv("z15N50.csv") # Read the data file; result is a data.frame.
y = myData$y
# The y values are the component named y.
Ntotal = length(y) # Compute the total number of flips.
dataList = list(
  # Put the information into a list.
  y = y ,
  Ntotal = Ntotal
)

#8.2.2
model {
  for ( i in 1:Ntotal ) {
    y[i]~ dbern( theta )
  }
  theta~ dbeta( 1 , 1 )
}


model {
  theta~dbeta( 1 , 1 )
  for ( i in 1:Ntotal ) {
    y[i]~ dbern( theta )
  }
}


modelString = " # open quote for modelString
model {
for ( i in 1:Ntotal ) {
y[i]~ dbern( theta )
}
theta~ dbeta( 1 , 1 )
}
" # close quote for modelString

writeLines( modelString , con="TEMPmodel.txt" ) # write to file


#8.2.3

thetaInit = sum(y)/length(y)
initsList = list( theta=thetaInit )

initsList = function() {
  resampledY = sample( y , replace=TRUE )
  thetaInit = sum(resampledY)/length(resampledY)
  thetaInit = 0.001+0.998*thetaInit
  return( list( theta=thetaInit ) )
}

y = c(rep(0,25),rep(1,75))

initsList()

#8.2.4

jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList ,
                        n.chains=3 , n.adapt=500 )

update( jagsModel , n.iter=500 )

codaSamples = coda.samples( jagsModel , variable.names=c("theta") ,
                            n.iter=3334 )

#8.2.5

source("DBDA2E-utilities.R")

diagMCMC( codaObject=codaSamples , parName="theta" )

#8.2.5.1

plotPost( codaSamples[,"theta"] , main="theta" , xlab=bquote(theta) )
plotPost( codaSamples[,"theta"] , main="theta" , xlab=bquote(theta) ,
          cenTend="median" , compVal=0.5 , ROPE=c(0.45,0.55) , credMass=0.90 )

#8.3

# Load the data
myData = read.csv("z15N50.csv") # must have a component named y
# Load the functions genMCMC, smryMCMC, and plotMCMC:
source("Jags-Ydich-Xnom1subj-MbernBeta.R")
# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=10000 )
# Display diagnostics of chain, for specified parameter:
diagMCMC( mcmcCoda , parName="theta" )
# Display numerical summary statistics of chain:
smryMCMC( mcmcCoda )
# Display graphical posterior information:
plotMCMC( mcmcCoda , data=y )

#8.4

myData = read.csv("z6N8z2N7.csv")
y = myData$y
s = as.numeric(myData$s) # converts character to consecutive integer levels

Ntotal = length(y)
Nsubj = length(unique(s))
dataList = list(
  y = y ,
  s = s ,
  Ntotal = Ntotal ,
  Nsubj = Nsubj
)

model {
  for ( i in 1:Ntotal ) {
    y[i]~ dbern( theta[s[i]] )
  }
  for ( s in 1:Nsubj ) {
    theta[s]~ dbeta(2,2)
  }
}

# Load the data
myData = read.csv("z6N8z2N7.csv") # myData is a data frame.
# Load the functions genMCMC, smryMCMC, and plotMCMC:
source("Jags-Ydich-XnomSsubj-MbernBeta.R")
# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=10000 )
# Display diagnostics of chain, for specified parameter:
diagMCMC( mcmcCoda , parName="theta[1]" )
# Display numerical summary statistics of chain:
smryMCMC( mcmcCoda , compVal=NULL , compValDiff=0.0 )
# Display graphical posterior information:
plotMCMC( mcmcCoda , data=myData , compVal=NULL , compValDiff=0.0 )


#8.5

dataList = list(
  # y = y ,
  s = s ,
  Ntotal = Ntotal ,
  Nsubj = Nsubj
)

#8.6.1

spy[i] <- pdf( y[i] , parameters ) / C # where pdf is a formula
1~dbern( spy[i] )

data {
  C <- 10000 # JAGS does not warn if too small!
  for (i in 1:N) {
    ones[i] <- 1
  }
}
model {
  for (i in 1:N) {
    spy[i] <- pdf( y[i] , parameters )/C # where pdf is a formula
    ones[i] ~ dbern( spy[i] )
  }
  parameters~ dprior...
}

#8.7

install.packages("runjags")

library("runjags")

nChains=3
nAdaptSteps=1000
nBurninSteps=500
nUseSteps=10000 # total number of used steps
nThinSteps=2

library(rjags)
jagsModel = jags.model(
  file="model.txt" ,
  data=dataList ,
  inits=initsList ,
  n.chains=nChains ,
  n.adapt=nAdaptSteps )
update(
  jagsModel ,
  n.iter=nBurninSteps )
codaSamples = coda.samples( jagsModel ,
                            variable.names=c("theta") ,
                            n.iter=ceiling(nUseSteps*nThinSteps/nChains) ,
                            thin=nThinSteps )

library(runjags)
runJagsOut <- run.jags( method="parallel" ,
                        model="model.txt" ,
                        monitor=c("theta") ,
                        data=dataList ,
                        inits=initsList ,
                        n.chains=nChains ,
                        adapt=nAdaptSteps ,
                        burnin=nBurninSteps ,
                        sample=ceiling(nUseSteps/nChains) ,
                        thin=nThinSteps ,
                        summarise=FALSE ,
                        plots=FALSE )
codaSamples = as.mcmc.list( runJagsOut )


