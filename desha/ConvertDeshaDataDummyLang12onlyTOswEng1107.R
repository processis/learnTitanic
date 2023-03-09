# Read Desharnais77 public dataset from promise uottawa repository
desharnais <- read.table("deshRandomFrShelly1106.csv",
                         sep = ",", header = TRUE)
desha <- subset(desharnais,Project!=76)
# continue to remove 4 rows, because not enough data in these projects
desha <- subset(desha,Project!=67)
desha <- subset(desha,Project!=45)
desha <- subset(desha,Project!=40)
### because ? in some fields under these columns 
#   has to specify as numeric
desha$TeamExp<-as.numeric(desha$TeamExp)
desha$ManagerExp<-as.numeric(desha$ManagerExp)
# Keep only Project+Effor + 8 independent var, remove yearEnd, PointsN , Adjust
desha <- desha[,c(1,2,3,(5:8),(11:14))]
### randomly keep 6 project rows , remaining 71 rows, keep as Training set
deshaTrain <- subset(desha,Project!=1)
deshaTrain <- subset(deshaTrain,Project!=2)
deshaTrain <- subset(deshaTrain,Project!=3)
deshaTrain <- subset(deshaTrain,Project!=4)
deshaTrain <- subset(deshaTrain,Project!=5)
deshaTrain <- subset(deshaTrain,Project!=6)
### keep the 6 rows in Test dataset
deshaTest <- subset(desha,Project < 7)
### remove first column Project from both Train and Test sets
deshaTrain <- deshaTrain[,c(2:11)]
deshaTest <- deshaTest[,c(2:11)]
deshaTrainY = deshaTrain$Effort
deshaTestY = deshaTest$Effort
#remove Effort and Lang3 from X set
deshaTrainX = deshaTrain[,c(1:3,5:9)]
deshaTestX = deshaTest[,c(1:3,5:9)]
swEngTestY = deshaTestY
swEngTrainY = deshaTrainY
swEngTrainX = deshaTrainX
swEngTestX = deshaTestX
swEngTrainXtrans = deshaTrainX
swEngTestXtrans = deshaTestX
###
#
#
deshaLog1 <- lm(Effort ~ PointsAjust+Transactions + Entities, data = deshaTrain)
summary(deshaLog1)
deshaLog1$coefficients
coef(deshaLog1)
require(coefplot)
coefplot(deshaLog1)