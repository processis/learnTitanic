library(shiny)
library(data.table)
library(shinydashboard)

ui = dashboardPage(
  dashboardHeader(title = "如何上传数据"),
  dashboardSidebar(
    menuItem("上传数据",tabName = "a"),
    br(),
    menuItem("head结果",tabName = "b"),
    br(),
    menuItem("summary结果",tabName = "c"),
    br(),
    menuItem("col",tabName = "d"),
    br(),
    menuItem("col1",tabName = "e")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "a",fileInput("dat","上传csv文件",accept = ".csv")),
      tabItem(tabName = "b",tableOutput('head')),
      tabItem(tabName = "c",verbatimTextOutput("summary")),
      tabItem(tabName = "d",tableOutput('col')),
      tabItem(tabName = "e",plotOutput('col1'))
    )
  )
)




