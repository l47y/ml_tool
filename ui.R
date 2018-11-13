source('config.R')
ui <- dashboardPage(
  
  dashboardHeader(
    title = 'ml_tool'
  ),
  
  dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                menuItem('Select and Filter Data', tabName = 'SelectandFilterData',  icon = icon('database')),
                menuItem('Show Data', tabName = 'ShowData', icon = icon('align-justify', lib = 'glyphicon')),
                menuItem('Overview', tabName = 'Overview', icon = icon('eye')),
                menuItem('Clean Data', tabName = 'Cleandata', icon = icon('wrench', lib = 'glyphicon')),
                menuItem('Graphical Analysis', tabName = 'GraphicalAnalysis', icon = icon('edit')),
                menuItem('Text Analysis', tabName = 'Textanalysis', icon = icon('font')),
                menuItem('Correlations and OHE', tabName = 'Correlations', icon = icon('indent-right', lib = 'glyphicon')),
                menuItem('Feature Selection', tabName = 'Featureselection', icon = icon('share', lib = 'glyphicon')),
                menuItem('Learn Model', tabName = 'LearnModel', icon = icon('random', lib = 'glyphicon')),
                menuItem('Compare Models', tabName = 'CompareModels', icon = icon('signal', lib = 'glyphicon')),
                menuItem('Documentation', tabName = 'Documentation', icon = icon('list-alt', lib = 'glyphicon'))
                
    )
  ),
     
  dashboardBody( 
    shinyDashboardThemes(
      #theme = "grey_dark"
      theme = 'purple_gradient'
      #theme = 'boe_website'
    ),
    tabItems(
      
      ######################################################################################  
      
      tabItem(
        tabName = 'SelectandFilterData',
        fluidPage(
          box(title = 'Select file', collapsible = T, width = 3, solidHeader = T,
              selectInput('sourceoffile', 'Where to get data from?', 
                          choices = c('Upload data from computer', 'Choose example data')),
              uiOutput('datafile'),
              uiOutput('importexampledata')
          ),
          box(title = 'Delete columns', collapsible = T, width = 3, solidHeader = T,
              uiOutput('whichcolumnsdelete'),
              actionButton('applydelete', 'Apply Deletion')
          ),
          box(title = 'Filter data', collapsible = T, solidHeader = T, width = 2, 
              actionButton('applyfilter', 'Apply Filter'),
              textOutput('initGlobaldata')
          ),
          box(title = 'Reset to original data', collapsible = T, width = 2, solidHeader = T,
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
      
      tabItem(
        tabName = 'ShowData', 
        fluidPage(
          DT::dataTableOutput('viewdata')
        )
      ),
      
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
      
      tabItem(
        tabName = 'Cleandata',
        fluidPage(
          infoBoxOutput('infoboxNAcolumns'),
          column(12,
            box(title = 'Remove missings in character columns', solidHeader = T, width = 4, collapsible = T,
                uiOutput('selectNAcharactercolumns'),
                selectInput('strategyNAchar', 'Select strategy to treat NAs', 
                            choices = c('Most frequent'))
            ),
            box(title = 'Remove missings in numeric columns', solidHeader = T, width = 4, collapsible = T,
                uiOutput('selectNAnumericcolumns'),
                selectInput('strategyNAnum', 'Select strategy to treat NAs', 
                            choices = c('Median', 'Mean', 'Set to zero'))
            ),
            box(title = 'Impute and Reset', solidHeader = T, width = 3, collapsible = T,
                actionButton('imputeNA', 'Impute NAs'),
                actionButton('resetNaTreats', 'Reset Imputing')
            ),
            column(12, 
              box(title = 'Remove constant features', solidHeader = T, width = 3, collapsible = T,
                  actionButton('removeconstantfeatures', 'Remove constant features')
              ),
              box(title = 'Normalize Features', collapsed = T, width = 3, solidHeader = T, 
                  uiOutput('selectcolsfornormalize'), 
                  selectInput('selectmethodfornormalize', 'Select method', 
                              choices = unlist(normalizing_dict$keys())),
                  actionButton('normalizefeatures', 'Normalize Features'), 
                  actionButton('resetnormalizing', 'Reset Normalizing')
              )
            )
          )
        )
      ),

      ######################################################################################  
      
      tabItem(
        tabName = 'GraphicalAnalysis', 
        fluidPage(
          box(title = 'Type of Graph and Settings', collapsible = T, solidHeader = T, width = 3,
              selectInput('selectgraphtype', 'Select Type', choices = c('Scatter', 'Network')),
              uiOutput('plotsettings1'),
              uiOutput('plotsettings2')
          ),
          column(12, 
            box(title = 'Plot', collapsible = T, solidHeader = T, width = 8,
                plotlyOutput('graphanaplot', height = 750, width = 750)
            )
          )
        ) 
      ),
      
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
          box(title = 'Delete high correlated features', width = 3, collapsible = T, solidHeader = T, 
              sliderInput('thresholdfordeletehighcorr', 'Threshold for Deletion', min = 0, max = 0.99, value = 0.8),
              actionButton('deletehighcorr', 'Delete correlated features')
          ),
          column(12,
            box(title = 'Correlations between variables', width = 8, collapsible = T, solidHeader = T,
              plotlyOutput('correlationplot')
            )
          )
          
        )
      ),
      
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
      
      tabItem(
        tabName = 'Featureselection',
        fluidPage(
          box(title = 'Settings for feature selection', collapsible = T, solidHeader = T, width = 4,
            selectInput('featureselectmethod', 'Select method', 
                        choices = unlist(featimp_dict$keys())),
            uiOutput('featureselectthreshold'),
            actionButton('calcfeatureimp', 'Get feature importance'),
            actionButton('deletefeaturesbythresh', 'Delete least n important features')
          ),
          box(title = 'Visualisation of feature importance', collapsible = T, solidHeader = T, width = 7,
              plotlyOutput('featimpplot')    
          )
        )
      ),
      
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
          box(title = 'Validation and testing', collapsible = T, solidHeader = T, width = 3,
              selectInput('choosevalidationstrat', 'Choose validation strategy', 
                          choices = c('Holdout', 'Cross validation', 'None')),
              uiOutput('parameterforvalidationstrat'),
              uiOutput('performancemeasures'),
              sliderInput('percentageoftestingdata', 'Choose percentage of final testing data', min = 0.01, max = 0.99, 
                          value = 0.2)
          ),
          box(title = 'Learn it!', collapsible = T, solidHeader = T, width = 3,
              actionButton('learnmodel', 'Learn model')
          ),
          column(12, 
            box(title = 'Performance overview', collapsible = T, solidHeader = T, width = 7,
                selectInput('showtrainortestperformance', 'What to show?', 
                            choices = c('Performance on training data', 'Performance on testing data')),
                plotlyOutput('plotperformanceoverview')
            ),
            box(title = 'Testing performance', collapsible = T, solidHeader = T, width = 3,
                shiny::tableOutput('testperformancetable')    
            )
          )
        )
      ),
      
      ######################################################################################  
      
      tabItem(
        tabName = 'CompareModels', 
        fluidPage(
          box(title = 'Model description', collapsible = T, width = 4, solidHeader = T,
              uiOutput('selectmodelfordescription'),
              htmlOutput('modeldescription')
          ),
          box(title = 'Model comparison', collapsible = T, width = 6, solidHeader = T,
              plotlyOutput('modelcomparisonplot')
          ),
          box(title = 'Delete models', collapsible = T, width = 2, solidHeader = T,
              actionButton('deletemodels', 'Delete')
          )
        )
      ),
      
      ######################################################################################  
        
      tabItem(
        tabName = 'Documentation',
        includeMarkdown('Documentation.Rmd')
      )
      
    ) 
  ) 
) 



