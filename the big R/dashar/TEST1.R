# 加载ggplot2包
library(ggplot2)

# 读取数据
#!desharnais123fillMedLog71TrainSet-loglength.csv
data <- read.csv("/media/user/娱乐/learnTitanic/the big R/dashar/!desharnais123fillMedLog71TrainSet.csv")

hist(data$Length)
hist(data$PointsAjust)

# 绘制散点图
ggplot(data, aes(x = data$LogPtsAjust, y = data$LogEffort, color = data$Language)) +
  geom_point() +
  labs(title = "Scatter plot of x vs y by group",
       x = "X Variable",
       y = "Y Variable",
       color = "Group")

data$Language<-as.factor(data$Language)

# 绘制散点图并指定颜色
ggplot(data, aes(x = data$LogPtsAjust, y = data$LogEffort, color = data$Language)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("1" = "red", "2" = "black", "3" = "green")) +
  labs(title = "Scatter plot of LogPtsAjust vs LogEffort by Language",
       x = "LogPtsAjust",
       y = "LogEffort",
       color = "Language") +
  theme_minimal()
