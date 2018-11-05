source('config.R')

shinyServer(function(input, output, session) {
  
  ######################################################################################  REACTIVES
  ######################################################################################  REACTIVES
  
  ######################################################################################  DATA STUFF
  
  globaldata <- reactiveVal(NULL)
  model <- reactiveVal(NULL)
  
  getdata <- reactive({
    file <- input$datafile
    if (is.null(file)) {
      return(NULL) 
    }
    globaldata(read_csv(file$datapath))
    read_csv(file$datapath)
  })
  
  getdatadeleted <- reactive({
    data <- globaldata()
    if (is.null(input$whichcolumnsdelete) == F) {
      data %<>% select(-one_of(input$whichcolumnsdelete))
    }
    globaldata(data)
    data
  })

  getdatafiltered <- reactive({
    data <- getdatadeleted()
    for (col in colnames(data)) {
      if (is.null(input[[col]])) {next()}
      if (class(data %>% pull(col)) == 'character') {
        data %<>% filter(!! as.name(col) %in% input[[col]])
      } else if (class(data %>% pull(col)) %in% c('numeric', 'integer')) {
        datcol <- data %>% pull(!! as.name(col))
        ind <- (datcol >= input[[col]][1] & datcol <= input[[col]][2])
        ind <- ind %in% c(T, NA)
        data <- data[ind, ]
      }
    }
    globaldata(data)
    data
  })

  ######################################################################################  MISSING DATA STUFF
  
  getnumbermissings <- reactive({
    sapply(globaldata(), function(x) sum(is.na(x)))
  })
  
  getcharnas <- reactive({
    charcols <- get_colsoftype(getdatafiltered(), 'character')
    nas <- getnumbermissings() 
    intersect(names(nas[nas > 0]), charcols)
  })
  
  getnumnas <- reactive({
    numcols <- get_colsoftype(getdatafiltered(), c('numeric', 'integer'))
    nas <- getnumbermissings() 
    intersect(names(nas[nas > 0]), numcols)
  })
  
  ######################################################################################  COUNT FILTERS
  
  getnumberfilters <- reactive({
    count <- 0
    for (col in colnames(getdata())) {
      if (class(getdata() %>% pull(col)) == 'character') {
        if (is.null(input[[col]]) == F) {count <- count + 1}
      } else if (class(getdata() %>% pull(col)) %in% c('numeric', 'integer')) {
        colrange <- range(getdata() %>% pull(col), na.rm = T)
        if (input[[col]][1] != colrange[1]) {count <- count + 1}
        if (input[[col]][2] != colrange[2]) {count <- count + 1}
      }
    }
    count
  })
  
  ######################################################################################  MODEL STUFF
  
  prepareDataForLearn <- eventReactive(input$learnmodel, {
    tmpdat <- globaldata() %>% make_strToFactors() %>% make_conformColnames()
    dummiealgos <- paste0(gettaskPrefix(),  c('lm'))
    if (getalgo() %in% dummiealgos) {
      tmpdat %<>% createDummyFeatures()
    }
    tmpdat
  })
  
  gettaskPrefix <- reactive({
    case_when(input$learnchoosetask == 'Regression' ~ 'regr.',
              input$learnchoosetask == 'Classification' ~ 'classif.')
  })
  
  getalgo <- reactive({
    algo <- learningalgos_dict[[input$learnchoosealgo]]
    paste0(gettaskPrefix(), algo)
  })
  
  learnmodel <- eventReactive(input$learnmodel, {
    
    # check whether NAs exist and if they can be handled by the algorithm
    nasPossible <- listLearners() %>% 
      filter(short.name == learningalgos_dict[[input$learnchoosealgo]]) %>% 
      pull(missings)
    if (mean(getnumbermissings()) != 0) {
      validate(need(nasPossible == T, 'This algorithm cant handle NAs. Please remove them.'))
    }
    
    # create a task 
    if (input$learnchoosetask == 'Regression') {
      task = makeRegrTask('task', data = prepareDataForLearn(), target = input$learnchoosetarget)
    } else if (input$learnchoosetask == 'Classification') {
      task = makeClassifTask('task', data = prepareDataForLearn(), target = input$learnchoosetarget)
    }
    
    # create a validation desc 
    if (input$choosevalidationstrat == 'Holdout'){
      desc <- makeResampleDesc(method = 'Holdout', split = input$parameterforvalidationstrat)
    } else if (input$choosevalidationstrat == 'Cross validation') {
      desc <- makeResampleDesc(method = 'CV', iters = input$parameterforvalidationstrat)
    }
    
    # create a list of parameters for the algorithm
    params <- algos_dict[[learningalgos_dict[[input$learnchoosealgo]]]]$parameter
    if (is_empty(params) == F) {
      for (param in names(params)) {
        if (input[[param]] != '') {
          params[[param]] <- as.numeric(input[[param]])
        }
      }
      params <- params[!unlist(lapply(params, is.null))]
    }

    # create learner 
    learner <- makeLearner(cl = getalgo(), par.vals = params)
    
    # train models if the validation strategy is not None
    if (input$choosevalidationstrat != 'None') {
      resample <- resample(learner, task, desc, 
                           measures = lapply(list(input$performancemeasures), 
                                             function(str){eval(parse(text = str))}))
    } else {
      resample <- list('measures.test' = NULL)
    }
    
    # train a final model on all data and return stuff
    model = train(task = task, learner = learner)
    list('model' = model, 'learner' = learner, 'task' = task, 'resample' = resample$measures.test)
  })
  
  ######################################################################################  OBSERVE
  ######################################################################################  OBSERVE
  
  observeEvent(input$imputeNAchar, {
    
    if (is.null(input$selectNAcharactercolumns) == F) {
      charnas <- getcharnas()
      if (input$selectNAcharactercolumns == 'ALL COLUMNS') {
        colsToUse <- charnas
      } else {
        colsToUse <- input$selectNAcharactercolumns
      }
      naAction <- case_when(input$strategyNAchar == 'Most frequent' ~ 'na.most_freq')
      tmpdat <- globaldata() %>% impute_at(.na = eval(parse(text = naAction)), .vars = colsToUse)
      globaldata(tmpdat)
      updateSelectInput(session, inputId = 'selectNAcharactercolumns',
                        choices = setdiff(charnas, colsToUse))
    }
  })
  
  observeEvent(input$imputeNAnum, {
    
    if (is.null(input$selectNAnumericcolumns) == F) {
      numnas <- getnumnas()
      if (input$selectNAnumericcolumns == 'ALL COLUMNS') {
        colsToUse <- numnas
      } else {
        colsToUse <- input$selectNAnumericcolumns
      }
      naAction <- case_when(input$strategyNAnum == 'Median' ~ 'na.median',
                            input$strategyNAnum == 'Mean' ~ 'na.mean',
                            input$strategyNAnum == 'Set to zero' ~ 'na.zero')
      tmpdat <- globaldata() %>% impute_at(.na = eval(parse(text = naAction)), .vars = colsToUse)
      globaldata(tmpdat)
      updateSelectInput(session, inputId = 'selectNAnumericcolumns',
                     choices = setdiff(numnas, colsToUse))
    }
  })
  
  observeEvent(input$resetNaTreats, {
    globaldata(getdata())
  })
  
  observeEvent(input$resetall, {
    updateSelectInput(session, inputId = 'whichcolumnsdelete', selected = NULL, choices = colnames(getdata()))

    numcols <- get_colsoftype(getdata(), c('numeric', 'integer'))
    charcols <- get_colsoftype(getdata(), 'character')

    lapply(numcols, function(col) {
      colrange <- range(getdata() %>% pull(col), na.rm = T)
      updateSliderInput(session, inputId = col, value = c(colrange[1], colrange[2]))
    })

    lapply(charcols, function(col) {
      updateSelectInput(session, inputId = col, selected = NULL, choices = unique(getdata() %>% pull(col)))
    })
  })
  
  observeEvent(input$ohe, {
    tmpdat <- globaldata() 
    colsWithManyFactors <- delete_colsWithManyFactors(tmpdat, input$maxfactorsOHE, onlyNames = T)
    allPossiblycols <- get_colsoftype(tmpdat, 'character')
    OHEcols <- setdiff(allPossiblycols, colsWithManyFactors)
    tmpdat <- createDummyFeatures(tmpdat %>% make_strToFactors(), cols = OHEcols)
    if (input$deleteothersOHE == T) {
      tmpdat %<>% select(-one_of(colsWithManyFactors))
    }
    globaldata(tmpdat)
  })
  
  observeEvent(input$resetohe, {
    globaldata(getdata())
  })
  
  ######################################################################################  OUTPUTS
  ######################################################################################  OUTPUTS
  
  ######################################################################################  SELECT AND FILTER
  
  output$charfilters <- renderUI({
    charcols <- get_colsoftype(getdatadeleted(), 'character')
    lapply(charcols, function(col) {
      selectInput(col, col, choices = unique(getdata() %>% pull(col)), multiple = T, selected = NULL)
    })
  })
  
  output$numfilters <- renderUI({
    numcols <- get_colsoftype(getdatadeleted(), c('numeric', 'integer'))
    lapply(numcols, function(col) {
      colrange <- range(getdata() %>% pull(col), na.rm = T)
      sliderInput(col, col, min = colrange[1], max = colrange[2], 
                  value = c(colrange[1], colrange[2]))
    })
  })
  
  output$whichcolumnsdelete <- renderUI({
    selectInput('whichcolumnsdelete', 'Choose columns to delete', choices = colnames(getdata()),
                multiple = T, selected = NULL)
  })
  
  ######################################################################################  View data
  
  output$viewdata <- DT::renderDataTable({
    datatable(getdatafiltered(), options = list(
      autowidth = F, scrollY = T, searching = T, searchHighlight = T, pageLength = 20
    ))
  })
  
  ######################################################################################  OVERVIEW
  
  output$infoboxOriginalSize <- renderInfoBox({
    infoBox('Size of original data', 
            HTML(paste0(dim(getdata())[1], ' rows<br>', 
                        dim(getdata())[2], ' columns')),
            color = 'purple', width = 3)
  })
  
  output$infoboxSize <- renderInfoBox({
    infoBox('Size of filtered data', 
            HTML(paste0(dim(globaldata())[1], ' rows<br>', 
                        dim(globaldata())[2], ' columns')),
            color = 'olive', width = 3)
  })
  
  output$infoboxNumberfilters <- renderInfoBox({
    infoBox('Active filters', getnumberfilters(), color = 'aqua', width = 3)
  })
  
  output$missingvalues <- renderPlotly({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    nas <- round(getnumbermissings() / dim(getdatafiltered())[1], digits = 4)
    nas <- nas[nas > 0]
    validate(need(is_empty(nas) == F, 'No NAs in data.'))
    plot_ly(y = orderXfactors(names(nas), nas, decr = F), x = nas, type = 'bar', color = 'pink') %>%
      add_plotlayout() %>%
      layout(title = 'Percentage of missing data')
      
  })
  
  output$selectcolfordist <- renderUI({
    selectInput('selectcolfordist', label = 'Select column', choices = colnames(getdatafiltered()))
  })
  
  output$distplot <- renderPlotly({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    plot_ly(x = globaldata() %>% pull(input$selectcolfordist), type = 'histogram', color = 'pink') %>%
      add_plotlayout() %>% 
      layout(title = paste0('Distribution of ', input$selectcolfordist))
  })
  
  ######################################################################################  CLEAN DATA
  
  output$infoboxNAcolumns <- renderInfoBox({
    nas <- getnumbermissings()
    numberNas <- length(nas[nas > 0])
    infoBox('Number of columns with NAs', numberNas, color = 'olive')
  })
  
  output$selectNAcharactercolumns <- renderUI({
    selectInput('selectNAcharactercolumns', 'Select columns to remove NAs', 
                choices = c('ALL COLUMNS', getcharnas()), multiple = T)
  })
  
  output$selectNAnumericcolumns <- renderUI({
    selectInput('selectNAnumericcolumns', 'Select columns to remove NAs', 
                choices = c('ALL COLUMNS', getnumnas()), multiple = T)
  })
  
  ######################################################################################  DETAILED ANALYSIS
  
  output$selectColumn1plot1 <- renderUI({
    selectInput('selectColumn1plot1', 'Select 1st column', choices = colnames(getdatadeleted()), 
                selected = NULL, width = NULL )
  })
  
  output$selectColumn2plot1 <- renderUI({
    selectInput('selectColumn2plot2', 'Select 2nd column', choices = colnames(getdatadeleted()), 
                selected = NULL, width = NULL)
  })
  
  output$plotidea2 <- renderPlotly({

  })
  
  ######################################################################################  CORRELATIONS
  
  output$correlationplot <- renderPlotly({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    mat <- get_cormat(getdatafiltered(), maxFactor = maxFactorsForCor, NAtoZero = T)
    showNotification(paste0('One hot encoding is used for character columns. If more than ', maxFactorsForCor,
                            ' factors in a column, than this column will not be considered.'),  
                     type = 'message', duration = 10)
    plot_ly(x = colnames(mat), y = colnames(mat), z = round(mat, digits = 4), 
            type = 'heatmap', colors = 'PiYG') %>% add_plotlayout() %>%
      layout(title = 'Correlations')
  })
  
  ######################################################################################  TEXT ANALYSIS
  
  output$selectcolumnfortextcloud <- renderUI({
    selectInput('selectcolumnfortextcloud', 'Select column for wordcloud',
                choices = get_colsoftype(getdatadeleted(), 'character'),
                selected = NULL, width = 250)
  })
  
  output$textcloud <- renderWordcloud2({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    wordtab <- get_wordfreq(getdatafiltered() %>% pull(input$selectcolumnfortextcloud))
    wordcloud2(wordtab, backgroundColor = 'grey48', size = input$textcloudsize, color = "random-light")
  })
  
  ######################################################################################  LEARN MODEL
  
  output$learnchoosealgo <- renderUI({
    if (input$learnchoosetask == 'Regression') {
      selectInput('learnchoosealgo', 'Choose algorithm', 
                  choices = c('Linear Regression', 
                              'Decision tree'))
    } else if (input$learnchoosetask == 'Classification') {
      selectInput('learnchoosealgo', 'Choose algorithm', 
                  choices = c('Logistic Regression', 
                              'Decision tree'))
    }
  })
  
  output$learnchoosetarget <- renderUI({
    selectInput('learnchoosetarget', 'Choose target variable', choices = colnames(getdatadeleted()))
  })
  
  output$parametersofalgo <- renderUI({
    algo <- learningalgos_dict[[input$learnchoosealgo]]
    params <- algos_dict[[algo]]$parameter
    validate(need(is.null(params) == F, 'This algorithm has no parameters to choose.'))
    lapply(names(params), function(param){
      textInput(inputId = param, label = param, value = '')
      })
  })
  
  output$parameterforvalidationstrat <- renderUI({
    
    if (input$choosevalidationstrat != 'None') {
      if (input$choosevalidationstrat == 'Holdout') {
        sliderInput('parameterforvalidationstrat', 'Choose Percentage of training data', 
                    min = 0, max = 1, value = 0.66)
      } else if (input$choosevalidationstrat == 'Cross validation') {
        sliderInput('parameterforvalidationstrat', 'Choose number of folds', 
                    min = 2, max = 10, value = 5)
      }
    }
  })
  
  output$performancemeasures <- renderUI({
    if (input$choosevalidationstrat != 'None') {
      if (input$learnchoosetask == 'Regression') {
        selectInput('performancemeasures', 'Performance measures', 
                    choices = c('mse', 'rsq'), multiple = T, selected = 'rsq')
      } else if (input$learnchoosetask == 'Classification') {
        selectInput('performancemeasures', 'Performance measures', 
                    choices = c('acc'), selected = 'acc')
      } 
    }
  })
  
  output$plotperformanceoverview <- renderPlotly({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    preds <- predict(learnmodel()$model, newdata = prepareDataForLearn()) 
    if (input$learnchoosetask == 'Regression') {
      p <- plot_ly(x = preds$data$truth, y = preds$data$response, type = 'scatter', mode = 'markers',
              color = 'pink')
    } else if(input$learnchoosetask == 'Classification') {
      preds <- preds$data %>% group_by(response, truth) %>% summarize(n = n())
      p <- plot_ly(x = preds$response, y = preds$truth, z = preds$n, type = 'heatmap', colors = 'PiYG') 
    }
    p %<>% add_plotlayout() %>% layout(xaxis = list(title = 'Target'), yaxis = list(title = 'Prediction'))
  })
  
  output$testperformancetable <- renderTable({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    validate(need(is.null(learnmodel()$resample) == F, 'No validation selected.'))
    learnmodel()$resample
  })

  ######################################################################################  DOCUMENTATION
  
  output$inc <- renderUI({
    getPage('Documentation.html')
  })

  ###################################################################################### THE END.
  ###################################################################################### THE END.
  
})


