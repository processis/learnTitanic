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
  titlePanel("基础数据分析"),
  tabsetPanel(
    tabPanel("数据上传与分析",
             titlePanel("数据上传"),
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
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
                 
               ),
               mainPanel(
                 h3("上传的数据"),
                 DTOutput("data_table"),
                 h3("基础分析"),
                 verbatimTextOutput("summary")
               )
             )
    ),
    
    
    
    #直方图
    
    tabPanel("直方图",
             pageWithSidebar(
               headerPanel('histogram 直方图'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 #selectInput('xcol2', 'X Variable', ""),
                 #selectInput('ycol2', 'Y Variable', "", selected = "")
                 
                 #numericInput("n","NUmber of observations:",value=100),
                 #submitButton("Update Plot")
                 
                 
                 #fileInput("file", "上传CSV文件", accept = c(".csv")),
                 selectInput("column", "选择列", choices = NULL),
                 sliderInput("binwidth", "直方图宽度", min = 0.1, max = 10, value = 1)
                 
                 
                 
                 
                 
                 
               ),
               mainPanel(
                 plotOutput("histogram")
               )
             )
    ),
    
    
    
    
    #散点图
    tabPanel("散点图",
             pageWithSidebar(
               headerPanel('Scatter plot 散点图'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput("PLOTxvar", "选择X轴变量", choices = NULL),
                 selectInput("PLOTyvar", "选择Y轴变量", choices = NULL),
                 actionButton("plot", "绘制散点图")
               ),
               mainPanel(
                 plotOutput("scatterplot"),
                 verbatimTextOutput("regression_equation")  # 显示回归方程
               )
             )
    ),
    
    
    
    
    
    
    #回归分析
    tabPanel("回归分析",
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
    
    tabPanel("相关性热力图",
             pageWithSidebar(
               headerPanel('Correlation Analysis相关性分析'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 # selectInput('xcol6', 'X Variable', ""),
                 # selectInput('ycol6', 'Y Variable', "", selected = ""),
                 # actionButton("choice", "incorporate external information"),
                 #selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
                 uiOutput("variable_select"),  # 动态生成变量选择器
                 actionButton("COLanalyze", "进行相关性分析")
                 
               ),
               mainPanel(
                 h4("相关性矩阵"),
                 tableOutput("correlation_matrix"),  # 显示相关性矩阵
                 h4("热力图"),
                 plotOutput("heatmap")  # 显示热力图
                 
                 #selectInput("columns", "Select Columns", choices = NULL), # no choices before uploading 
                 
               )
             )
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  )
)
)