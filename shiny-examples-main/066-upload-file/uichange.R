fluidPage(
  titlePanel("Uploading Files上传文件"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload选择要上载的文件',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header标头', TRUE),
      radioButtons('sep', 'Separator分离器',
                   c(Comma逗号=',',
                     Semicolon分号=';',
                     Tab换行='\t'),
                   ','),
      radioButtons('quote', 'Quote引用',
                   c(None无='',
                     'Double Quote双引号'='"',
                     'Single Quote单引号'="'"),
                   '"'),
      tags$hr(),
      p('If you want a sample .csv or .tsv file to upload,',
        'you can first download the sample',
        a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
        a(href = 'pressure.tsv', 'pressure.tsv'),
        'files, and then try uploading them.'
      )
    ),
    mainPanel(
      tableOutput('contents')
    )
  )
)
