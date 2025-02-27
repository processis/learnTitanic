# 读取数据
data <- read.csv("/media/user/娱乐/learnTitanic/the big R/dashar/!desharnais123fillMedLog71TrainSet-loglength.csv")

# 计算Pearson相关性矩阵
cor_matrix <- cor(data, method = "pearson", use = "complete.obs")

# 打印相关性矩阵
print(cor_matrix)

# 可视化相关性矩阵
library(corrplot)
corrplot(cor_matrix, method = "circle")

# 计算特定变量之间的相关性
correlation <- cor(data$LogLength, data$LogEffort, method = "pearson")
print(correlation)

# 计算相关性的显著性
cor_test_result <- cor.test(data$var1, data$var2, method = "pearson")
print(cor_test_result)

# 保存相关性矩阵
write.csv(cor_matrix, file = "correlation_matrix.csv")