library(shiny)
library(datasets)
library(DT)
library(ggplot2)
library(reshape2)  # 用于数据重塑
library(corrplot)  # 用于绘制相关性热力图
library(glmnet)  # 用于岭回归分析

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
               headerPanel('散点图'),
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
               headerPanel('直方图'),
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
               headerPanel('My col'),
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
               headerPanel('回归分析'),
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
    
    
    
    
    
    #热力图
    
    tabPanel("sixth Type",
             pageWithSidebar(
               headerPanel('热力图'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                # selectInput('xcol6', 'X Variable', ""),
                # selectInput('ycol6', 'Y Variable', "", selected = ""),
                # actionButton("choice", "incorporate external information"),
                 #selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
                 checkboxInput("remove_non_numeric", "移除非数值列", value = TRUE)
               ),
               mainPanel(
                 plotOutput("correlation_plot")
                 
                 #selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
                
               )
             )
    ),
    
    
    
    #岭回归分析
    tabPanel("senventh Type",
             pageWithSidebar(
               headerPanel('回归分析'),
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
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  )
)
)