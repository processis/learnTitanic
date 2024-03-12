x <- c(3,4,5)
x <- c(3,4,5)
x
ls()
sum(x)
mean(x)
var(x)


capacity <- c(14,13,14,13,16,NA,NA,20,NA)
mean(capacity)
mean(capacity, na.rm=TRUE)
is.na(capacity)

substr(x, start, stop)
#Extract or replace substrings in a character vector
substr("abcdefg",2,4)

substr("abcdefg",4,2)
substr("abcdefg",stop=4,start=2)

substr(stop=4, start=2,"abcdefg")

myfunction <- function(argument1, argument2, ... ){
  statements
  return(object)
}



sumofsquares <- function(x,y,z){
  sumsq <- sum(c(x^2,y^2,z^2))
  return(sumsq)
}

sumofsquares(3,4,5)

sumofsquares <- function(x,y,z){ # list the arguments
  sumsq <- sum(c(x^2,y^2,z^2)) # perform the operations
  return(sumsq)
  # return the value
}

x <- c(3,4,5)
stooges <- c("Moe","Larry", "Curly")
z <- c(T,F,T,F,T,T)
stooges[2]


y <- seq(10,30)
y

y <- 10:30
y

w <- seq(10,30,2)
w

trifecta <- c(6,8,2)
repeated_trifecta <- rep(trifecta,4)
repeated_trifecta


repeated_trifecta <- rep(trifecta,x)
repeated_trifecta

num_matrix <- seq(5,100,5)
dim(num_matrix) <-c(5,4)
num_matrix

t(num_matrix)

num_matrix <- matrix(seq(5,100,5),nrow=5)
num_matrix

num_matrix <- matrix(seq(5,100,5),nrow=5,byrow=T)
num_matrix

num_matrix[5,4]


eye_color <- c(2,2,4,1,5,5,5,6,1,3,6,3,1,4)
feye_color <- factor(eye_color)
levels(feye_color) <- c("amber","blue", "brown","gray","green",
                        "hazel")
feye_color



empathy_score <- c(15,21,45,32,61,74,53,92,83,22,67,55,42,44)
eyes_and_empathy <- list(eyes_code=eye_color, eyes=feye_color,
                         empathy=empathy_score)
eyes_and_empathy

eyes_and_empathy$empathy
eyes_and_empathy$empathy[4]

t.result <- t.test(eyes_and_empathy$empathy, mu = 30)
t.result

t.result$data.name
t.result$p.value
t.result$statistic

e <- data.frame(eye_color,feye_color,empathy_score)
e

e[7,3]
e[7,]
edit(e)

e.blue <- e$empathy_score[e$feye_color=="blue"]
e.green <- e$empathy_score[e$feye_color=="green"]
e.hazel <- e$empathy_score[e$feye_color=="hazel"]

e.averages <- c(mean(e.blue),mean(e.green),mean(e.hazel))
e.amounts <- c(length(e.blue), length(e.green),
               length(e.hazel))
colors <- c("blue","green","hazel")
e.averages.frame <- data.frame(color=colors,
                               average=e.averages, n=e.amounts)
e.averages.frame



library("MASS", lib.loc="C:/Program Files/R/R-3.3.1/library")
edit(anorexia)
anorexia$Postwt-anorexia$Prewt
t.test(anorexia$Postwt-anorexia$Prewt, mu=0)


install.packages("ggplot2")
library("ggplot2", lib.loc="~/R/win-library/3.3")

install.packages("ggplot2")
function(dependent_var ~ independent_var, data=data_frame)
aov(Postwt-Prewt ~ Treat, data=anorexia)

read.<format>("File Name", arg1, arg2, ...)
write.<format>(dataframe, "File Name", arg1, arg2, ...)
scores_frame <- read.xlsx("C:/Spreadsheets/Scores.xlsx",
                          sheetName="Sheet1")
scores_frame

scores_frame$Math_Score[4]
write.xlsx(anorexia,"C:/Spreadsheets/anorexia.xlsx")

read.csv("C:/CSVFiles/Scores.csv")

write.csv(anorexia,"C:/CSVFiles/anorexia.csv")

read.table("C:/TextFiles/ScoresText.txt", header=TRUE)

write.table(anorexia, "C:/TextFiles/anorexia.txt", quote =
              FALSE, sep = "\t")
read.xlsx("Scores.xlsx","Sheet1")






