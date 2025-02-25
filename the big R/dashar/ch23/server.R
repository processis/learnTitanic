server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    #xia la kuang xuan xiang
    #yu UI dui ying
    
    
    
    
    
    
    return(df)
  })
  

  
  # 显示数据表
  output$data_table <- renderDT({
    datatable(data())
  })
  
  # 显示基础分析结果
  output$summary <- renderPrint({
    df <- data()
    summary(df)
  })
  
  
  #生成 直方图
  # 动态生成列选择器
  # 更新列选择器
  observeEvent(data(), {
    updateSelectInput(session, "column", choices = names(data()))
  })
  
  # 绘制直方图
  output$histogram <- renderPlot({
    req(input$column)
    ggplot(data(), aes_string(x = input$column)) +
      geom_histogram(binwidth = input$binwidth, fill = "blue", color = "black") +
      labs(title = paste("Histogram of", input$column),
           x = input$column,
           y = "Count")
  })
  ################
  
  # 动态生成列选择器
  output$column_selector3 <- renderUI({
    req(data())
    selectInput("column", "选择要筛选的列", choices = names(data()))
  })
  
  # 动态生成筛选条件选择器
  output$filter_selector3 <- renderUI({
    req(input$column)
    column_data <- data()[[input$column]]
    if (is.numeric(column_data)) {
      sliderInput("filter", "选择数值范围", min = min(column_data), max = max(column_data), value = c(min(column_data), max(column_data)))
    } else {
      selectInput("filter", "选择类别", choices = unique(column_data), multiple = TRUE)
    }
  })
  
  # 根据筛选条件显示部分数据表
  output$filtered_table <- renderDT({
    req(input$column, input$filter)
    filtered_data <- data()
    column_data <- filtered_data[[input$column]]
    if (is.numeric(column_data)) {
      filtered_data <- filtered_data[column_data >= input$filter[1] & column_data <= input$filter[2], ]
    } else {
      filtered_data <- filtered_data[column_data %in% input$filter, ]
    }
    datatable(filtered_data)
  })
  
  
  
  #散点图
  
  # 更新变量选择
  observeEvent(data(), {
    updateSelectInput(session, "PLOTxvar", choices = names(data()))
    updateSelectInput(session, "PLOTyvar", choices = names(data()))
  })
  
  # 计算回归方程
  regression_model <- reactive({
    req(input$PLOTxvar, input$PLOTyvar)
    lm(as.formula(paste(input$PLOTyvar, "~", input$PLOTxvar)), data = data())
  })
  
  # 绘制散点图和回归线
  output$scatterplot <- renderPlot({
    req(input$PLOTxvar, input$PLOTyvar)
    ggplot(data(), aes_string(x = input$PLOTxvar, y = input$PLOTyvar)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal()
  })
  
  # 显示回归方程
  output$regression_equation <- renderPrint({
    req(regression_model())
    model <- regression_model()
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    cat("回归方程:\n")
    cat(paste(input$PLOTyvar, "=", round(slope, 2), "*", input$PLOTxvar, "+", round(intercept, 2))
    )})
  

  
  
  
  
  #回归分析
  
  info <- eventReactive(input$choice, {
    req(data())
    f <- data()
    f
  })
  
  observeEvent(input$choice, {  ## to update only when you click on the actionButton
    req(info())
    updateSelectInput(session,"independent", "Please Select independent Variable(s):", choices = names(info()) )
  })
  
  
  # output$Table_selected.col <- renderTable({
  #   input$choice
  #   req(info(),input$columns)
  #   f = info()
  #   f = subset(f, select = input$columns) #subsetting takes place here
  #   head(f)
  # })
  
  output$dependent1 = renderUI({
    req(data(),input$independent)
    radioButtons("dependent1", "Select a dependent Variable:",choices=names(data())[!names(data()) %in% input$independent])
  })
  
  ###  need to build your formuila correctly; It will work with multiple independent variables
  ###  model <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = RegData)})
  
  runRegression <- reactive({
    req(data(),input$independent,input$dependent1)
    lm(reformulate(input$independent, input$dependent1), data=data())
    # multinom(reformulate(input$independent, input$dependent1), data=mydf())  ### mulitnomial from nnet package
  })
  
  output$regTab = renderPrint({
    req(runRegression())
    if(!is.null(input$independent)){
      summary(runRegression())
    } else {
      print(data.frame(Warning="Please select Model Parameters."))
    }
  })
  
  
  
  #热力图
  
  
  
  # 动态生成变量选择器
  output$variable_select <- renderUI({
    req(data())
    numeric_vars <- names(data())[sapply(data(), is.numeric)]  # 仅选择数值型变量
    checkboxGroupInput("selected_vars", "选择变量进行相关性分析", choices = numeric_vars)
  })
  
  # 计算相关性矩阵
  correlation_matrix <- reactive({
    req(input$selected_vars)
    selected_data <- data()[, input$selected_vars, drop = FALSE]
    cor(selected_data, use = "complete.obs")  # 计算相关性矩阵
  })
  
  # 显示相关性矩阵
  output$correlation_matrix <- renderTable({
    req(correlation_matrix())
    correlation_matrix()
  }, rownames = TRUE)
  
  # 绘制热力图
  output$heatmap <- renderPlot({
    req(correlation_matrix())
    melted_cormat <- melt(correlation_matrix())  # 将相关性矩阵转换为长格式
    ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "", fill = "Correlation")
  })
  
  
  
  
  
  
  
  
  
  
  
}
)