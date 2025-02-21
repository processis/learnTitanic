
library(shiny)
library(datasets)
library(DT)
library(ggplot2)
library(reshape2)  # 用于数据重塑
library(corrplot)  # 用于绘制相关性热力图
library(glmnet)  # 用于岭回归分析
library(e1071)
library(pls)
library(earth)
library(dplyr)
library(rmarkdown)
library(knitr)

ui <- fluidPage(
  titlePanel("数据分析与PDF报告导出"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上传数据文件 (CSV)", accept = c(".csv")),
      selectInput("variable", "选择分析变量", choices = NULL),
      actionButton("analyze", "分析数据"),
      br(), br(),
      downloadButton("downloadReport", "导出PDF报告")
    ),
    
    mainPanel(
      h3("数据摘要"),
      verbatimTextOutput("summary"),
      h3("数据分布图"),
      plotOutput("plot")
    )
  )
)















server <- function(input, output, session) {
  
  # 读取上传的数据
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # 更新变量选择
  observeEvent(data(), {
    updateSelectInput(session, "variable", choices = names(data()))
  })
  
  # 数据分析
  analysis_results <- eventReactive(input$analyze, {
    req(input$variable)
    var <- data()[[input$variable]]
    
    # 描述性统计
    summary_stats <- summary(var)
    
    # 绘图
    plot <- ggplot(data(), aes(x = !!sym(input$variable))) +
      geom_histogram(fill = "blue", bins = 30) +
      labs(title = paste("Distribution of", input$variable))
    
    # 返回结果
    list(summary = summary_stats, plot = plot)
  })
  
  # 显示数据摘要
  output$summary <- renderPrint({
    analysis_results()$summary
  })
  
  # 显示数据分布图
  output$plot <- renderPlot({
    analysis_results()$plot
  })
  
  # 生成并导出PDF报告
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("analysis_report", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # 创建临时R Markdown文件
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # 提取分析结果
      summary_stats <- analysis_results()$summary
      plot <- analysis_results()$plot
      
      # 渲染报告为PDF
      rmarkdown::render(
        tempReport,
        output_file = file,
        output_format = "pdf_document",
        params = list(
          summary = summary_stats,
          plot = plot,
          variable = input$variable
        )
      )
    }
  )
}


# 运行Shiny应用程序
shinyApp(ui = ui, server = server)