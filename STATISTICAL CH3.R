edit(Cars93)

graphics_function(data, arg1, arg2, ...)
hist(Cars93$Price)
xlab= "Price (x $1,000)"

xlim = c(0,70)
main = "Prices of 93 Models of 1993 Cars"
hist(Cars93$Price, xlab="Price (x $1,000)", xlim = c(0,70),
     main = "Prices of 93 Models of 1993 Cars")
probability = True
hist(Cars93$Price, xlab="Price (x $1,000)", xlim = c(0,70),
     main = "Prices of 93 Models of 1993 Cars",probability
     = TRUE)
lines(density(Cars93$Price))

table(Cars93$Type)
barplot(table(Cars93$Type))
ylim = c(0,25)
xlab = "Type"
ylab = "Frequency"

axis.lty = "solid"
space = .05
barplot(table(Cars93$Type),ylim=c(0,25), xlab="Type",
        ylab="Frequency", axis.lty = "solid", space = .05)

pie(table(Cars93$Type))
dotchart(x, labels, arg1, arg2 ...)
type.frame <- data.frame(table(Cars93$Type))
type.frame
dotchart(type.frame$Freq,type.frame$Var1)
dotchart(type.frame[,2],type.frame[,1])

rev.values <-
  c(1000,1300,1300,1100,1400,800,1200,1500,1850,
    2330,860,1300,1400,1600,1970,570,380,450,465,580,
    155,190,210,250,300)
space.rev <- matrix(rev.values,nrow=5,byrow = T)

colnames(space.rev) <-
  c("1990","1991","1992","1993","1994")
rownames(space.rev) <- c("Commercial Satellites
  Delivered","Satellite Services","Satellite Ground
  Equipment","Commercial Launches","Remote Sensing Data")
space.rev

color.names = c("black","grey25","grey50","grey75","white")
barplot(space.rev, beside = T, xlab= "Year",ylab= "Revenue
(X $1,000)", col=color.names)

legend(1,2300,rownames(space.rev), cex=0.7, fill =color,
       names, bty = "n")
plot(Cars93$Horsepower, Cars93$MPG.city,
     xlab="Horsepower",ylab="MPG City", main ="MPG City vs
  Horsepower")
plot(Cars93$MPG.city ~ Cars93$Horsepower,
     xlab="Horsepower",ylab="MPG City", main ="MPG City vs
   Horsepower")
plot(Cars93$Horsepower,Cars93$MPG.city, xlab="Horsepower",
     ylab="MPG City", main = "MPG City vs Horsepower",pch=16)
pch = as.character(Cars93$Cylinders)
plot(Cars93$Horsepower,Cars93$MPG.city, xlab="Horsepower",
     ylab="MPG City", main = "MPG City vs Horsepower", pch
     = as.character(Cars93$Cylinders))
cars.subset <- subset(Cars93, select = c(MPG,
                                         city,Price,Horsepower))

head(cars.subset)

pairs(cars.subset)
boxplot(Cars93$Horsepower ~ Cars93$Cylinders, xlab="Cylinders",
        ylab="Horsepower")
boxplot(Horsepower ~ Cylinders, data = Cars93,
        xlab="Cylinders", ylab="Horsepower")

ggplot(Cars93, aes(x=Price))
ggplot(Cars93, aes(x=Price)) +
  geom_histogram()
geom_histogram(binwidth=5, color = "black", fill = "white")
labs(x = "Price (x $1000)", y="Frequency",title="Prices of 93
  Models of 1993 Cars")

ggplot(Cars93, aes(x=Price)) +
  geom_histogram(binwidth=5,color="black",fill="white") +
  labs(x = "Price (x $1000)", y="Frequency", title="Prices of
93 Models of 1993 Cars")

ggplot(Cars93, aes(x=Type))+
  geom_bar() +
  labs(y="Frequency", title="Car Type and Frequency in Cars93")


type.frame <- data.frame(table(Cars$93.Type))
colnames(type.frame)<- c("Type","Frequency")
type.frame
ggplot(type.frame, aes(x=Frequency,y= Type))
ggplot(type.frame, aes(x=Frequency,y=Type)) +
  geom_point()

ggplot(type.frame,aes(x=Frequency,y=reorder(Type,Frequency))
       

geom_point(size =4)
theme_bw() +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(color = "black",
                                        linetype = "dotted"))
labs(y= "Type")
ggplot(type.frame, aes(x=Frequency,y=reorder(Type,Frequency))) +
  geom_point(size = 4) +
  theme_bw() +
  theme(panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_line(color = "black",linetype
                                        = "dotted"))+
  labs(y="Type")

space.rev

space.melt <- melt(space.rev)
head(space.melt)
colnames(space.melt) <- c("Industry","Year","Revenue")
head(space.melt)


ggplot(space.melt, aes(x=Year,y=Revenue,fill=Industry))
geom_bar(stat = "identity", position = "dodge", color ="black")

scale_fill_grey(start = 0,end = 1)
labs(y="Revenue (X $1,000)")
theme_bw()
theme(panel.grid.major.x = element_blank())
ggplot(space.melt, aes(x=Year,y=Revenue,fill=Industry)) +
  geom_bar(stat = "identity", position = "dodge", color="black") +
  scale_fill_grey(start = 0,end = 1)+
  labs(y="Revenue (X $1,000)")+
  theme_bw()+
  theme(panel.grid.major.x = element_blank())

ggplot(Cars93,aes(x=Horsepower,y=MPG.city))+
  geom_point()
ggplot(Cars93, aes(x=Horsepower, y=MPG.city, label = Cylinders))
ggplot(Cars93, aes(x = Horsepower,y = MPG.city,label =
                     Cylinders)) +
  geom_text()
theme(panel.grid=element_blank())
ggplot(Cars93, aes(x=Horsepower, y=MPG.city, label=Cylinders)) +
  geom_text() +
  theme_bw() +
  theme(panel.grid=element_blank())


cars.subset <- subset(Cars93, select = c(MPG.
                                         city,Price,Horsepower))
ggpairs(cars.subset)

cars.subset <- subset(Cars93, select = c(MPG.city,Price,
                                         Horsepower,Cylinders))
ggpairs(cars.subset)


ggplot(Cars93, aes(x=Cylinders, y= Horsepower))
ggplot(Cars93, aes(x=Cylinders,y=Horsepower)) +
  geom_boxplot()

ggplot(Cars93, aes(x=Cylinders,y=Horsepower)) +
  geom_boxplot()+
  geom_point()


gplot(Cars93, aes(x=Cylinders,y=Horsepower)) +
  geom_boxplot()+
  geom_point()+
  geom_jitter()































