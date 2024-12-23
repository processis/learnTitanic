#CHAPTER 27

# 27.1 AMonteCarlo experiment

rnos <- runif(100)
which(rnos<= 0.1)

which(rnos<= 0.1)[1]

death1 <- numeric(30)
for (i in 1:30){
  rnos <- runif(100)
  death1[i] <- which(rnos<= 0.1)[1]
}
death1

1/mean(death1)

death2 <- numeric(30)
for (i in 1:30) {
  rnos <- runif(100)
  death2[i] <- which(rnos<= 0.2)[1] }

1/mean(death2)

death <- c(death1,death2)
factory <- factor(c(rep(1,30),rep(2,30)))


plot(factory,death,xlab="factory",ylab="age at failure",col="wheat3")

model1 <- glm(death~factory,Gamma)
summary(model1)

rm(death)

# 27.10 Parametric analysis


seedlings <- read.table("/media/user/娱乐/learnTitanic/the R book-sec/seedlings.txt",header=T)


attach(seedlings)
names(seedlings)


library(survival)

status <- 1*(death>0)

plot(survfit(Surv(death,status)~1),ylab="Survivorship",xlab="Weeks",col=4)

model <- survfit(Surv(death,status)~cohort)
summary(model)

plot(model,col=c("red","blue"),ylab="Survivorship",xlab="week")

model

#27.11 Cox’s proportional hazards

model1 <- coxph(Surv(death,status)~strata(cohort)*gapsize)
summary(model1)

model2 <- coxph(Surv(death,status)~strata(cohort)+gapsize)
anova(model1,model2)

summary(model2)

# 27.12 Models with censoring

rm(status)
detach(seedlings)

cancer <- read.table("/media/user/娱乐/learnTitanic/the R book-sec/cancer.txt",header=T)
attach(cancer)
names(cancer)

plot(survfit(Surv(death,status)~treatment),
     col=c(1:4),ylab="Surivorship",xlab="Time")


tapply(death[status==1],treatment[status==1],mean)

tapply(death[status==1],treatment[status==1],var)

model1 <- survreg(Surv(death,status)~treatment,dist="exponential")
summary(model1)

model2 <- survreg(Surv(death,status)~treatment)
summary(model2)

anova(model1,model2)

treat2 <- treatment
levels(treat2)

levels(treat2)[1:2] <- "DrugsAB"
levels(treat2)

model3 <- survreg(Surv(death,status)~treat2)
anova(model2,model3)

levels(treat2)[2:3] <- "placeboC"
model4 <- survreg(Surv(death,status)~treat2)
anova(model3,model4)

summary(model4)

tapply(predict(model4,type="response"),treat2,mean)

tapply(death[status==1],treat2[status==1],mean)

detach(cancer)
rm(death, status)

insects <- read.table("/media/user/娱乐/learnTitanic/the R book-sec/roaches.txt",header=T)
attach(insects)
names(insects)

plot(survfit(Surv(death,status)~group),col=c(2,3,4),ylab="Survivorship",
     xlab="Time")

model1 <- survreg(Surv(death,status)~weight*group,dist="exponential")
summary(model1)

model2 <- survreg(Surv(death,status)~weight*group)
summary(model2)

anova(model1,model2)

model3 <- step(model2)

summary(model3)

tapply(predict(model3),group,mean)

tapply(death[status==1],group[status==1],mean)

tapply(death,group,mean)

model10 <- coxph(Surv(death,status)~weight*group)
summary(model10)

model11 <- step(model10)

summary(model11)

tapply(death[status==1],group[status==1],mean)

12.61/8.02

12.61/9.57



