ibrary(shiny)
library(ggplot2)

# 创建Shiny应用程序界面
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      # 在侧边栏中添加用户输入控件（如数据上传或选择）
      # 这里省略了用户输入的部分
      div(
        fileInput("csvFile", "Choose defferential analysis result File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        )
    ),
    )
    mainPanel(
      # 在主面板中添加绘图输出
      plotOutput("residualPlot")
    )
  )
)