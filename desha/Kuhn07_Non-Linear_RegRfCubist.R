################################################################################
### R code from Applied Predictive Modeling (2013) by Kuhn and Johnson.
### Copyright 2013 Kuhn and Johnson
### Web Page: http://www.appliedpredictivemodeling.com
### Contact: Max Kuhn (mxkuhn@gmail.com)
###
### Chapter 7: Non-Linear Regression Models
###
### Required packages: AppliedPredictiveModeling, caret, doMC (optional), earth,
###                    kernlab, lattice, nnet
###
### Data used: The swEngubility from the AppliedPredictiveModeling package
###
### Notes: 
### 1) This code is provided without warranty.
###
### 2) This code should help the user reproduce the results in the
### text. There will be differences between this code and what is is
### the computing section. For example, the computing sections show
### how the source functions work (e.g. randomForest() or plsr()),
### which were not directly used when creating the book. Also, there may be 
### syntax differences that occur over time as packages evolve. These files 
### will reflect those changes.
###
### 3) In some cases, the calculations in the book were run in 
### parallel. The sub-processes may reset the random number seed.
### Your results may slightly vary.
###
################################################################################

################################################################################
### Load the data
#
# 10.24 added RandomForest, and Cubist from 08_Regression Tree,
#        and write testResults to ...RfCubist.csv
#
library(AppliedPredictiveModeling)
#data(swEngubility)

### Create a control funciton that will be used across models. We
### create the fold assignments explictily instead of relying on the
### random number seed being set to identical values.

library(caret)
set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)

################################################################################
### Section 7.1 Neural Networks

### Optional: parallel processing can be used via the 'do' packages,
### such as doMC, doMPI etc. We used doMC (not on Windows) to speed
### up the computations.

### WARNING: Be aware of how much memory is needed to parallel
### process. It can very quickly overwhelm the availible hardware. We
### estimate the memory usuage (VSIZE = total memory size) to be 
### 2677M/core.

library(doMC)
registerDoMC(2)


library(caret)

nnetGrid <- expand.grid(decay = c(0, 0.01, .1), 
                        size = c(1, 3, 5, 7, 9, 11, 13), 
                        bag = FALSE)

set.seed(100)
nnetTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                  method = "avNNet",
                  tuneGrid = nnetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"),
                  linout = TRUE,
                  trace = FALSE,
                  MaxNWts = 13 * (ncol(swEngTrainXtrans) + 1) + 13 + 1,
                  maxit = 1000,
                  allowParallel = FALSE)
nnetTune

plot(nnetTune)

testResults <- data.frame(obs = swEngTestY,
                          NNet = predict(nnetTune, swEngTestXtrans))

################################################################################
### Section 7.2 Multivariate Adaptive Regression Splines

set.seed(100)
marsTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                  method = "earth",
                  tuneGrid = expand.grid(degree = 1, nprune = 2:38),
                  trControl = ctrl)
marsTune

plot(marsTune)

testResults$MARS <- predict(marsTune, swEngTestXtrans)

marsImp <- varImp(marsTune, scale = FALSE)
plot(marsImp, top = 25)

################################################################################
### Section 7.3 Support Vector Machines

## In a recent update to caret, the method to estimate the
## sigma parameter was slightly changed. These results will
## slightly differ from the text for that reason.

set.seed(100)
svmRTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneLength = 14,
                  trControl = ctrl)
svmRTune
plot(svmRTune, scales = list(x = list(log = 2)))                 

svmGrid <- expand.grid(degree = 1:2, 
                       scale = c(0.01, 0.005, 0.001), 
                       C = 2^(-2:5))
set.seed(100)
svmPTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                  method = "svmPoly",
                  preProc = c("center", "scale"),
                  tuneGrid = svmGrid,
                  trControl = ctrl)

svmPTune
plot(svmPTune, 
     scales = list(x = list(log = 2), 
                   between = list(x = .5, y = 1)))                 

testResults$SVMr <- predict(svmRTune, swEngTestXtrans)
testResults$SVMp <- predict(svmPTune, swEngTestXtrans)

################################################################################
#write.table(testResults,file="testResults.csv",sep=",") # output testResults.csv

################################################################################
### Section 7.4 K-Nearest Neighbors

### First we remove near-zero variance predictors
#knnDescr <- swEngTrainXtrans[, -nearZeroVar(swEngTrainXtrans)]

#set.seed(100)
#knnTune <- train(x = knnDescr, y = swEngTrainY,
#                 method = "knn",
#                 preProc = c("center", "scale"),
#                 tuneGrid = data.frame(k = 1:20),
#                 trControl = ctrl)
                 
#knnTune

#plot(knnTune)

#testResults$Knn <- predict(svmRTune, swEngTestXtrans[, names(knnDescr)])

################################################################################
set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)
### Section 8.5 Random Forests

mtryGrid <- data.frame(mtry = floor(seq(10, ncol(swEngTrainXtrans), length = 10)))


### Tune the model using cross-validation
set.seed(100)
rfTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                method = "rf",
                tuneGrid = mtryGrid,
                ntree = 1000,
                importance = TRUE,
                trControl = ctrl)
rfTune

plot(rfTune)
testResults$RF <- predict(rfTune, swEngTestXtrans) #add RF predict results
rfImp <- varImp(rfTune, scale = FALSE)
rfImp
################################################################################
### Section 8.7 Cubist

cbGrid <- expand.grid(committees = c(1:10, 20, 50, 75, 100), 
                      neighbors = c(0, 1, 5, 9))

set.seed(100)
cubistTune <- train(swEngTrainXtrans, swEngTrainY,
                    "cubist",
                    tuneGrid = cbGrid,
                    trControl = ctrl)
cubistTune

plot(cubistTune, auto.key = list(columns = 4, lines = TRUE))

cbImp <- varImp(cubistTune, scale = FALSE)
cbImp

testResults$CUBIST <- predict(cubistTune, swEngTestXtrans) #add result of CUBIST

################################################################################
write.table(testResults,file="testResultsSvmRfCubist.csv",sep=",") # output testResults.csv

### Session Information

sessionInfo()

q("no")


