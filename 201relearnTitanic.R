# Load raw data
train <- read.csv("/media/user/1907USB/R3/titanic/titanic/train.csv", header = TRUE)
test <- read.csv("/media/user/1907USB/R3/titanic/titanic/test.csv", header = TRUE)
# Add a "Survived" variable to the test set to allow for combining data sets
test.Survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
# Combine data set
data.combined <- rbind(train, test.Survived)
# A bit about R data types (e.g., factors)
str(data.combined)
data.combined$Survived <- as.factor(data.combined$Survived)
View(train)
data.combined$Pclass <- as.factor(data.combined$Pclass)
# Take a look at gross survival rates
table(data.combined$Survived)
# Distribution across classes
table(data.combined$Pclass)
# Load up ggplot2 package to use for visualizations
library(ggplot2)
# Hypothesis - Rich folks survived at a higer rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
     geom_bar() +
     xlab("Pclass") +
     ylab("Total Count") +
     labs(fill = "Survived") 
View(train)
# Examine the first few names in the training data set
head(as.character(train$Name))
# How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))
# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
# Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]
# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)
# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]
# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")), ]
mrses[1:5,]
View(data.combined)
males <- data.combined[which(data.combined$Sex == "male"), ]
males[1:5,]
# Create a utility function to help with title extraction
extractTitle <- function(name) {
     name <- as.character(name)
  
      if (length(grep("Miss.", name)) > 0) {
             return ("Miss.")
         } else if (length(grep("Master.", name)) > 0) {
               return ("Master.")
           } else if (length(grep("Mrs.", name)) > 0) {
                 return ("Mrs.")
             } else if (length(grep("Mr.", name)) > 0) {
                  return ("Mr.")
                } else {
                    return ("Other")
              }
   }
titles <- NULL
for (i in 1:nrow(data.combined)) {
      titles <- c(titles, extractTitle(data.combined[i,"Name"]))
  }
data.combined$title <- as.factor(titles)
View(data.combined)
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
    geom_bar() +
     facet_wrap(~Pclass) + 
     ggtitle("Pclass") +
     xlab("Title") +
     ylab("Total Count") +
     labs(fill = "Survived")
# What's the distribution of females to males across train & test?
table(data.combined$Sex)


