# 创建Shiny应用程序服务器
server <- function(input, output) {
  
  data<- read.csv(inFile$datapath)
  # 定义绘图函数
  output$residualPlot <- renderPlot({
    # 从用户输入中获取数据（假设数据在输入中名为'data'）
    data <- input$data
    
    # 进行线性回归并提取残差
    fit <- lm(y ~ x, data = data)
    residuals <- resid(fit)
    
    # 创建散点图并添加回归线和残差点
    plot <- ggplot(data, aes(x = x, y = y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      geom_point(aes(y = residuals), color = "red")
    
    # 返回绘制好的图表
    plot
  })
}