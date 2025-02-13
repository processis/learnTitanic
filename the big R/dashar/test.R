# 加载必要的库
library(shiny)
library(DT)  # 用于显示交互式表格
library(dplyr)  # 用于数据处理

# 定义UI
ui <- fluidPage(
  titlePanel("上传数据表并选择不同数据和选项"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上传CSV文件", accept = c(".csv")),
      uiOutput("column_selector"),  # 选择列
      uiOutput("filter_selector"),  # 筛选条件
      uiOutput("sort_selector"),    # 排序选项
      actionButton("apply", "应用选项")
    ),
    
    mainPanel(
      DTOutput("data_table")  # 显示数据表
    )
  )
)

# 定义服务器逻辑
server <- (function(input, output, session) {
  
  # 读取上传的文件
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # 动态生成列选择器
  output$column_selector <- renderUI({
    req(data())
    selectInput("columns", "选择要显示的列", choices = names(data()), multiple = TRUE)
  })
  
  # 动态生成筛选条件选择器
  output$filter_selector <- renderUI({
    req(data(), input$columns)
    filter_inputs <- lapply(input$columns, function(col) {
      if (is.numeric(data()[[col]])) {
        sliderInput(paste0("filter_", col), paste("筛选", col), min = min(data()[[col]]), max = max(data()[[col]]), value = c(min(data()[[col]]), max(data()[[col]])))
      } else {
        selectInput(paste0("filter_", col), paste("筛选", col), choices = unique(data()[[col]]), multiple = TRUE)
      }
    })
        do.call(tagList, filter_inputs)
  })
    
    # 动态生成排序选项选择器
    output$sort_selector <- renderUI({
      req(data(), input$columns)
      selectInput("sort_column", "选择排序的列", choices = input$columns)
    })
    
    # 应用选项并显示数据表
    observeEvent(input$apply, {
      req(data(), input$columns, input$sort_column)
      
      # 筛选数据
      filtered_data <- data()
      for (col in input$columns) {
        filter_value <- input[[paste0("filter_", col)]]
        if (is.numeric(filtered_data[[col]])) {
          filtered_data <- filtered_data[filtered_data[[col]] >= filter_value[1] & filtered_data[[col]] <= filter_value[2], ]
        } else {
          filtered_data <- filtered_data[filtered_data[[col]] %in% filter_value, ]
        }
      }
      
      # 排序数据
      sorted_data <- filtered_data %>% arrange(across(all_of(input$sort_column)))
      
      # 显示数据表
      output$data_table <- renderDT({
        datatable(sorted_data[, input$columns, drop = FALSE])
      })
    })
})

# 运行Shiny应用程序
shinyApp(ui = ui, server = server)