library(AppliedPredictiveModeling)
data(AlzheimerDisease)
predictors$E2<-predictors$E3<-predictors$E4<-0
predictors$E2[grep1("2",predictors$Genotype)]<-1
predictors$E3[grep1("3",predictors$Genotype)]<-1
predictors$E4[grep1("4",predictors$Genotype)]<-1

set.seed(730)
split<-createDataPartiton(diagnosis,p=.8,list=FALSE)
adData<-predictors
adData$Class<-diagnosis
training<-adData[split,]
testing<-adData[-split,]
predVars<-names(adData)[!(names(adData)%in%c("Class","Genotype"))]

fiveStates<-function(...)c(twoClassSummary(...),
                           defaultSummary(...))

set.seed(104)
index<-createMultiFolds(training$Class,times=5)
varSeq<-seq(1,length(predVars)-1,by=2)


#KUHN-2

svmFuncs<-careFUnce
svmFuncs$summary<-fiveStates
ctrl<-rfeControl(method="repeatedcv",
                 repeate=5,
                 verbose=TRUE,
                 functions=svmFuncs,
                 index=index)
set.seed(721)
svmRFE<-rfe(x=training[,predVars],
            y=training$Class,
            sizes=varSeq,
            rfeControl=ctrl,
            method="svmRadial",
            tuneLength=12,
            preProc=c("center","scale"),
            trControl=trainControl(method="cv",
                                   verboseIter=FALSE,
                                   classProbs=true))
svmRFE

#kuhn-3

newRF<-rfFuncs
newRF$summary<-fiveStates

ctrl<-rfeCentrol(method="repeatedcv",
                 repeats=5,
                 verbose=TRUE,
                 function=newRF,
                 index=index)
set.seed(721)

rfRFE<-rfe(x=training[,predVars],
           y=training$Class,
           sizes=varSeq,
           setric="ROC",
           rfeControl=ctrl,
           ntree=1000)
rfRFE

#kuhn-4

initial<-alm(Class~tau+VEGF+E4+IL_3,data=training,
             family=binomial)
library(MASS)
stepAIC(initial,direction="both")


#kuhn-5

library(caret)
str(rfFuncs)
