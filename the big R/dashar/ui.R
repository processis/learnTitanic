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

ui <- shinyUI(fluidPage(
  titlePanel("Column Plot"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               
               
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                 radioButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"'),
                 
                 
                 radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                              inline = TRUE),
                 downloadButton('downloadReport')
                 
                 
                 
                 
                 
               ),
               mainPanel(
                 verbatimTextOutput("summar"),
                 tableOutput('contents')
                 
                 
               )
             )
    ),
    
    
    
    #散点图
    tabPanel("First Type",
             pageWithSidebar(
               headerPanel('Scatter plot 散点图'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")
                 
               ),
               mainPanel(
                 plotOutput('MyPlot')
               )
             )
    ),
    
    #直方图
    
    tabPanel("second Type",
             pageWithSidebar(
               headerPanel('histogram 直方图'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol2', 'X Variable', ""),
                 #selectInput('ycol2', 'Y Variable', "", selected = "")
                 
                 #numericInput("n","NUmber of observations:",value=100),
                 #submitButton("Update Plot")
                 
                 
                 #fileInput("file", "上传CSV文件", accept = c(".csv")),
                 uiOutput("column_selector2")
                 
                 
                 
                 
                 
               ),
               mainPanel(
                 plotOutput('Myhistogram')
               )
             )
    ),
    
    
    
    tabPanel("third Type",
             pageWithSidebar(
               headerPanel('select data'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 uiOutput("column_selector3"),
                 uiOutput("filter_selector3")
                 
               ),
               mainPanel(
                 DTOutput("filtered_table")
               )
             )
    ),
    
    
    
    
    
    
    
    
    #回归分析
    tabPanel("fifth Type",
             pageWithSidebar(
               headerPanel('regression analysis 回归分析'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 actionButton("choice", "Define Regression Variables"),
                 selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
               ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
                 uiOutput("dependent1"),
                 #tableOutput("Table_selected.col"),
                 verbatimTextOutput("regTab")
               )
             )
    ),
    
    
    
    
    
    #热力图 相关性分析
    
    tabPanel("sixth Type",
             pageWithSidebar(
               headerPanel('Correlation Analysis‌ 相关性分析'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                # selectInput('xcol6', 'X Variable', ""),
                # selectInput('ycol6', 'Y Variable', "", selected = ""),
                # actionButton("choice", "incorporate external information"),
                 #selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
                 checkboxInput("remove_non_numeric", "移除非数值列", value = TRUE),
                 uiOutput("var_select"),
                 actionButton("analyze", "分析")
               
                 ),
               mainPanel(
                 plotOutput("correlation_plot"),
                 verbatimTextOutput("correlation_result")
                 
                 #selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
                
               )
             )
    ),
    
    
    
    #岭回归分析
    tabPanel("senventh Type",
             pageWithSidebar(
               headerPanel(' ridge regression 岭回归分析'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 uiOutput("response_selector"),  # 选择响应变量
                 uiOutput("predictor_selector"), # 选择预测变量
                 sliderInput("lambda", "选择正则化参数 (lambda)", min = 0, max = 10, value = 1, step = 0.1),
                 actionButton("run", "运行岭回归")
               ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
                 verbatimTextOutput("summary"),  # 显示岭回归结果
                 plotOutput("coef_plot")        # 绘制系数图
               )
             )
    ),
    
    
    
    
    #SVM
    
    #SVM预测
    tabPanel("eighth Type",
             pageWithSidebar(
               headerPanel('SVM预测'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 actionButton("SVMpredict", "进行预测"),
                 selectInput("x_axis8", "选择X轴变量", choices = NULL), # 选择X轴变量
                 selectInput("y_axis8", "选择Y轴变量", choices = NULL)  # 选择Y轴变量
               ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
               
                 verbatimTextOutput("SVMprediction"),# 显示预测结果
                 plotOutput("SVMplot") # 显示绘图
               )
             )
    ),
    
    #偏最小二乘回归分析
    
    tabPanel("ninth Type",
             pageWithSidebar(
               headerPanel('Partial Least Squares 偏最小二乘回归分析'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 selectInput("plsresponse", "选择因变量", choices = NULL),
                 selectInput("plspredictors", "选择自变量", choices = NULL, multiple = TRUE),
                 actionButton("runpls", "运行PLS回归")
                 ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
                 
                 verbatimTextOutput("plssummary"),
                 plotOutput("plsplot")
               )
             )
    ),
    
    
    #多元自适应回归样条分析 (MARS)
    
    
    tabPanel("tenth Type",
             pageWithSidebar(
               headerPanel('MARS 多元自适应回归样条分析'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol5', 'X Variable', ""),
                 
                 #selectInput('ycol5', 'Y Variable', "", selected = "")
                 selectInput("marsresponse", "选择因变量", choices = NULL),
                 selectInput("marspredictors", "选择自变量", choices = NULL, multiple = TRUE),
                 actionButton("marsrun", "运行MARS分析")
               ),
               mainPanel(
                 #actionButton("choice", "Define Regression Variables"),
                 #selectInput("independent", "Independent Variables:", choices = NULL, multiple = T),
                 
                 
                 verbatimTextOutput("marssummary"),
                 plotOutput("marsplot")
               )
             )
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  )
)
)