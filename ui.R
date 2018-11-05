source('config.R')

ui <- dashboardPage(
  
  dashboardHeader(
    title = 'Tool'
  ),
  
  dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                menuItem('Select and Filter Data', tabName = 'SelectandFilterData',  icon = icon('database')),
                menuItem('View data', tabName = 'Viewdata', icon = icon('align-justify', lib = 'glyphicon')),
                menuItem('Overview', tabName = 'Overview', icon = icon('eye')),
                menuItem('Clean data', tabName = 'Cleandata', icon = icon('wrench', lib = 'glyphicon')),
                menuItem('Detailed analysis', tabName = 'Detailedanalysis', icon = icon('signal', lib = 'glyphicon')),
                menuItem('Correlations', tabName = 'Correlations', icon = icon('indent-right', lib = 'glyphicon')),
                menuItem('Feature selection', tabName = 'Featureselection', icon = icon('share', lib = 'glyphicon')),
                menuItem('Text analysis', tabName = 'Textanalysis', icon = icon('font')),
                menuItem('Learn model', tabName = 'LearnModel', icon = icon('random', lib = 'glyphicon')),
                menuItem('Documentation', tabName = 'Documentation', icon = icon('list-alt', lib = 'glyphicon'))
                
    )
  ),
     
  dashboardBody( 
    shinyDashboardThemes(
      #theme = "grey_dark"
      theme = 'purple_gradient'
    ),
    tabItems(
      
      ######################################################################################  
      ###################################################################################### 
      
      tabItem(
        tabName = 'SelectandFilterData',
        fluidPage(
          box(title = 'Select file', collapsible = T, width = 3, solidHeader = T,
              fileInput('datafile', 
                        'Choose Data',
                        multiple = FALSE,
                        accept = c('text/csv',
                                   'text/comma-separated-values,text/plain',
                                   '.csv'))
          ),
          box(title = 'Delete columns', collapsible = T, width = 3, solidHeader = T,
              uiOutput('whichcolumnsdelete')
          ),
          box(title = 'Reset', collapsible = T, width = 2, solidHeader = T,
              actionButton('resetall', 'Reset all')
          ),
          column(12, 
                 box(title = 'Filter character columns', collapsible = T, width = 4, solidHeader = T,
                     uiOutput('charfilters')
                 ),
                 box(title = 'Filter numeric columns', collapsible = T, width = 4, solidHeader = T,
                     uiOutput('numfilters')
                 )
          )
        )
      ),
      
      ######################################################################################  
      ###################################################################################### 
      
      tabItem(
        tabName = 'Viewdata', 
        fluidPage(
          DT::dataTableOutput('viewdata')
        )
      ),
      
      ######################################################################################  
      ###################################################################################### 
      
      tabItem(
        tabName = 'Overview',
        fluidPage(
          column(12,
            infoBoxOutput('infoboxOriginalSize'),
            infoBoxOutput('infoboxSize'),
            infoBoxOutput('infoboxNumberfilters')
          ),
          column(12, 
            box(title = 'Missing values', collapsible = T, width = 4, solidHeader = T,
              plotlyOutput('missingvalues')
            ),
            box(title = 'Distribution of values', collapsible = T, width = 8,
                uiOutput('selectcolfordist'),
                plotlyOutput('distplot')
            )
          )
        )
      ),
      
      ######################################################################################  
      ###################################################################################### 
      
      tabItem(
        tabName = 'Cleandata',
        fluidPage(
          infoBoxOutput('infoboxNAcolumns'),
          column(12,
            box(title = 'Character columns', solidHeader = T, width = 4, collapsible = T,
                uiOutput('selectNAcharactercolumns'),
                selectInput('strategyNAchar', 'Select strategy to treat NAs', 
                            choices = c('Most frequent', 'According to distribution')),
                actionButton('imputeNAchar', 'Impute NAs')
            ),
            box(title = 'Numeric columns', solidHeader = T, width = 4, collapsible = T,
                uiOutput('selectNAnumericcolumns'),
                selectInput('strategyNAnum', 'Select strategy to treat NAs', 
                            choices = c('Median', 'Mean', 'Set to zero')),
                actionButton('imputeNAnum', 'Impute NAs')
            ),
            box(title = 'Reset', solidHeader = T, width = 2, collapsible = T,
                actionButton('resetNaTreats', 'Reset Imputing')
            )
          )
        )
      ),
      
      ######################################################################################  
      ###################################################################################### 
      
      tabItem(
        tabName = 'Detailedanalysis',
        fluidPage(
          box(title = 'Plot idea 1', solidHeader = T, width = 6, collapsible = T,
            uiOutput('selectColumn1plot1', inline = T),
            uiOutput('selectColumn2plot1', inline = T),
            plotlyOutput('plotidea1')
          ),
          box(title = 'Plot idea 2', solidHeader = T, width = 6, collapsible = T,
            plotlyOutput('plotidea2')    
          )
        )
      ),
      
      ######################################################################################  
      ###################################################################################### 
      
      tabItem(
        tabName = 'Correlations',
        fluidPage(
          box(title = 'One Hot Encoding', width = 3, collapsible = T, solidHeader = T, 
              sliderInput('maxfactorsOHE', 'Maximum number of factors to encode', 
                          min = 2, max = 100, value = 10),
              checkboxInput('deleteothersOHE', 'Delete columns with too many factors'),
              actionButton('ohe', 'Encode data'),
              actionButton('resetohe', 'Reset encoding')
          ),
          box(title = 'Correlations between variables', width = 8, collapsible = T, solidHeader = T,
            plotlyOutput('correlationplot')
          )
          
        )
      ),
      
      ######################################################################################  
      ###################################################################################### 
      
      tabItem(
        tabName = 'Textanalysis',
        fluidPage(
          box(title = 'Textcloud', collapsible = T, width = 7, solidHeader = T,
              uiOutput('selectcolumnfortextcloud'),
              wordcloud2Output('textcloud', height = 750, width = 750)
          ),
          box(title = 'Settings for Textcloud', collapsible = T, width = 4, solidHeader = T,
              sliderInput('textcloudsize', 'Font size for textcloud', min = 0.2, max = 5, value = 1)
          )
        )
      ),
      
      ######################################################################################  
      ######################################################################################  
      
      tabItem(
        tabName = 'LearnModel',
        fluidPage(
          box(title = 'Settings for model', collapsible = T, width = 3, solidHeader = T,
            selectInput('learnchoosetask', 'Choose task', choices = c('Regression', 'Classification')),
            uiOutput('learnchoosealgo'),
            uiOutput('learnchoosetarget')
           
          ),
          box(title = 'Algorithm parameters', collapsible = T, solidHeader = T, width = 3,
              uiOutput('parametersofalgo')
          ),
          box(title = 'Validation strategy', collapsible = T, solidHeader = T, width = 3,
              selectInput('choosevalidationstrat', 'Choose validation strategy', 
                          choices = c('Holdout', 'Cross validation', 'None')),
              uiOutput('parameterforvalidationstrat'),
              uiOutput('performancemeasures')
          ),
          box(title = 'Learn it!', collapsible = T, solidHeader = T, width = 3,
              actionButton('learnmodel', 'Learn model')
          ),
          column(12, 
            box(title = 'Performance overview', collapsible = T, solidHeader = T, width = 7,
                plotlyOutput('plotperformanceoverview')
            ),
            box(title = 'Testing performance', collapsible = T, solidHeader = T, width = 3,
                shiny::tableOutput('testperformancetable')    
            )
          )
        )
      ),
      
      ######################################################################################  
      ###################################################################################### 
        
      tabItem(
        tabName = 'Documentation',
        #htmlOutput("inc")
        includeMarkdown('Documentation.Rmd')
        #includeHTML('Documentation.html')
      )
      
    ) 
  ) 
) 



