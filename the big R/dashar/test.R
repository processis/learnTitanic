library(earth)

# 定义UI
ui <- fluidPage(
  titlePanel("多元自适应回归样条分析 (MARS)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上传数据文件 (CSV)", accept = c(".csv")),
      selectInput("marsresponse", "选择因变量", choices = NULL),
      selectInput("marspredictors", "选择自变量", choices = NULL, multiple = TRUE),
      actionButton("marsrun", "运行MARS分析")
    ),
    
    mainPanel(
      verbatimTextOutput("marssummary"),
      plotOutput("marsplot")
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  
  # 读取上传的数据
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # 更新因变量和自变量的选择
  observeEvent(data(), {
    updateSelectInput(session, "marsresponse", choices = names(data()))
    updateSelectInput(session, "marspredictors", choices = names(data()))
  })
  
  # 运行MARS分析
  mars_model <- eventReactive(input$marsrun, {
    req(input$marsresponse, input$marspredictors)
    formula <- as.formula(paste(input$marsresponse, "~", paste(input$marspredictors, collapse = "+")))
    earth(formula, data = data())
  })
  
  # 显示回归结果
  output$marssummary <- renderPrint({
    req(mars_model())
    summary(mars_model())
  })
  
  # 绘制回归结果图
  output$marsplot <- renderPlot({
    req(mars_model())
    plot(mars_model())
  })
}

# 运行Shiny应用程序
shinyApp(ui = ui, server = server)