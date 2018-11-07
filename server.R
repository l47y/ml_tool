source('config.R')

shinyServer(function(input, output, session) {
  
  ######################################################################################  REACTIVES
  ######################################################################################  REACTIVES
  
  ######################################################################################  DATA STUFF
  
  globaldata <- reactiveVal(NULL)
  unencodeddata <- reactiveVal(NULL)
  models <- list(NULL)
  countHowManyModels <- reactiveVal(1)
  filterlist <- reactiveVal(NULL)

  getdataOriginal <- reactive({
    file <- input$datafile
    if (is.null(file)) {
      return(NULL) 
    }
    read_csv(file$datapath)
  })
  
  getdata <- reactive({
    file <- input$datafile
    if (is.null(file)) {
      return(NULL) 
    }
    dat <- read_csv(file$datapath)
    globaldata(dat)
    filterlist(setNames(as.list(rep(F, ncol(dat))), colnames(dat)))
    dat
  })
  
  ######################################################################################  MISSING DATA STUFF
  
  getnumbermissings <- reactive({
    sapply(globaldata(), function(x) sum(is.na(x)))
  })
  
  getcharnas <- reactive({
    charcols <- get_colsoftype(globaldata(), 'character')
    nas <- getnumbermissings() 
    intersect(names(nas[nas > 0]), charcols)
  })
  
  getnumnas <- reactive({
    numcols <- get_colsoftype(globaldata(), c('numeric', 'integer'))
    nas <- getnumbermissings() 
    intersect(names(nas[nas > 0]), numcols)
  })
  
  ######################################################################################  FEATURE SELECTION STUFF
  
  getfeatimptable <- eventReactive(input$calcfeatureimp, {
    generateFilterValuesData(task = gettask(), learner = getlearner(), 
                             method = featimp_dict[[input$featureselectmethod]]$name)$data
  })
  
  ######################################################################################  MODEL STUFF
  
  prepareDataForLearn <- eventReactive(c(input$learnmodel, input$calcfeatureimp), ignoreInit = T, {
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
  
  gettask <- reactive({
    if (input$learnchoosetask == 'Regression') {
      task = makeRegrTask('task', data = prepareDataForLearn(), target = input$learnchoosetarget)
    } else if (input$learnchoosetask == 'Classification') {
      task = makeClassifTask('task', data = prepareDataForLearn(), target = input$learnchoosetarget)
    }
    task
  })
  
  getlearner <- reactive({
    
    #check if all introduced parameters are in the correct format
    paramSet <- getParamSet(getalgo())$pars
    params <- names(algos_dict[[learningalgos_dict[[input$learnchoosealgo]]]]$parameter)
    tryCorrectFormat <- list()
    for (param in params) {
      if (input[[param]] != '') {
        typeRequired <- paramSet[[param]]$type
        actualValue <- input[[param]]
        conversion <- paste0("as.", typeRequired)
        evalStr <- paste0(conversion, "(actualValue)")
        tryCorrectFormat[param] <- try(eval(parse(text = evalStr)))
      } else {
        tryCorrectFormat[param] <- 'dummie'
      }
    }
    couldNotBeConverted <- names(tryCorrectFormat[is.na(tryCorrectFormat)])
    validate(need(!length(couldNotBeConverted) > 0,
                 paste0('The following parameters are in the wrong formats: ',
                        paste(couldNotBeConverted, collapse = ' '))))
    
    # create a list of parameters for the algorithm and construct learner
    params <- algos_dict[[learningalgos_dict[[input$learnchoosealgo]]]]$parameter
    if (is_empty(params) == F) {
      for (param in names(params)) {
        if (input[[param]] != '') {
          params[[param]] <- as.numeric(input[[param]])
        }
      }
      params <- params[!unlist(lapply(params, is.null))]
    }
    makeLearner(cl = getalgo(), par.vals = params)
  })
  
  learnmodel <- eventReactive(input$learnmodel, {
    
    # check whether NAs exist and if they can be handled by the algorithm
    if (mean(getnumbermissings()) != 0) {
      nasPossible <- listLearners() %>% 
        filter(short.name == learningalgos_dict[[input$learnchoosealgo]]) %>% pull(missings)
      validate(need(nasPossible == T, 'This algorithm cant handle NAs. Please remove them.'))
    }
    
    # check whether multiclass can be handled by the algorithm if there are more than two classes
    if (input$learnchoosetask == 'Classification') {
      if (length(unique(globaldata() %>% pull(input$learnchoosetarget))) > 2) {
        multiclassPossible <- listLearners() %>% 
          filter(short.name == learningalgos_dict[[input$learnchoosealgo]]) %>% pull(multiclass)
        validate(need(multiclassPossible == T, 'This algorithm cant handle multiclass problems.'))
      }
    }
    
    # create a task 
    task <- gettask()
    
    # create a validation desc 
    if (input$choosevalidationstrat == 'Holdout'){
      desc <- makeResampleDesc(method = 'Holdout', split = input$parameterforvalidationstrat)
    } else if (input$choosevalidationstrat == 'Cross validation') {
      desc <- makeResampleDesc(method = 'CV', iters = input$parameterforvalidationstrat)
    }

    # create learner (parameters in getlearner function)
    learner <- getlearner()
    
    # train models if the validation strategy is not None
    if (input$choosevalidationstrat != 'None') {
      resample <- resample(learner, task, desc, 
                           measures = lapply(list(input$performancemeasures), 
                                             function(str){eval(parse(text = str))}))
    } else {
      resample <- list('measures.test' = NULL)
    }
    
    # train a final model and add it to the modellist of trained models
    model <- train(task = task, learner = learner)
    models[[countHowManyModels()]] <- list('model' = mode, 'resample' = resample$measures.test)
    countHowManyModels(countHowManyModels() + 1)
    print(resample$measures.test)
    list('model' = model, 'learner' = learner, 'task' = task, 'resample' = resample$measures.test)
  })
  
  ######################################################################################  OBSERVE
  ######################################################################################  OBSERVE
  
  observeEvent(input$applydelete, {
    data <- globaldata()
    if (is.null(input$whichcolumnsdelete) == F) {
      data %<>% select(-one_of(input$whichcolumnsdelete))
    }
    globaldata(data)
  })
  
  observeEvent(input$applyfilter, {
    data <- globaldata()
    whichFiltersAreSet <- filterlist()
    for (col in colnames(data)) {
      if (is.null(input[[col]])) {next()}
      if (class(data %>% pull(col)) %in% c('factor', 'character')) {
        data %<>% filter(!! as.name(col) %in% input[[col]])
        whichFiltersAreSet[[col]] <- T
        
      } else if (class(data %>% pull(col)) %in% c('numeric', 'integer')) {
        datcol <- data %>% pull(!! as.name(col))
        ind <- (datcol >= input[[col]][1] & datcol <= input[[col]][2])
        ind <- ind %in% c(T, NA)
        if (sum(ind) < nrow(data)) {
          whichFiltersAreSet[[col]] <- T
        }
        data <- data[ind, ]
      }
      
    }
    filterlist(whichFiltersAreSet)
    print(filterlist())
    globaldata(data)
  })
  
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
    
    updateSelectInput(session, inputId = 'whichcolumnsdelete', selected = NULL, choices = colnames(getdataOriginal()))

    numcols <- get_colsoftype(getdataOriginal(), c('numeric', 'integer'))
    charcols <- get_colsoftype(getdataOriginal(), 'character')

    lapply(numcols, function(col) {
      colrange <- range(getdataOriginal() %>% pull(col), na.rm = T)
      updateSliderInput(session, inputId = col, value = c(colrange[1], colrange[2]))
    })

    lapply(charcols, function(col) {
      updateSelectInput(session, inputId = col, selected = NULL, choices = unique(getdataOriginal() %>% pull(col)))
    })
    filterlist(setNames(as.list(rep(F, ncol(getdataOriginal()))), colnames(getdataOriginal())))
    globaldata(getdataOriginal())
  })
  
  observeEvent(input$ohe, {
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    tmpdat <- globaldata() 
    unencodeddata(globaldata())
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
    globaldata(unencodeddata())
  })
  
  observeEvent(input$deletefeaturesbythresh, {
    featuresToDelete <- get_leastImportanceFeatures(getfeatimptable(), input$featureselectthreshold)
    globaldata(globaldata() %>% select(-one_of(featuresToDelete)))
  })
  
  observeEvent(input$deletehighcorr, {
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    mat <- abs(get_cormat(globaldata()))
    mat[lower.tri(mat, diag = T)] <- 0
    whereHighCorrFeatures <- apply(mat, 1, function(row){any(row > input$thresholdfordeletehighcorr)})
    whichColsDelete <- names(whereHighCorrFeatures[whereHighCorrFeatures])
    globaldata(globaldata() %>% select(-one_of(whichColsDelete)))
  })

  ######################################################################################  OUTPUTS
  ######################################################################################  OUTPUTS
  
  ######################################################################################  SELECT AND FILTER
  
  output$charfilters <- renderUI({
    charcols <- get_colsoftype(globaldata(), c('character', 'factor'))
    lapply(charcols, function(col) {
      selectInput(col, col, choices = unique(globaldata() %>% pull(col)), multiple = T, selected = NULL)
    })
  })
  
  output$numfilters <- renderUI({
    numcols <- get_colsoftype(globaldata(), c('numeric', 'integer'))
    lapply(numcols, function(col) {
      colrange <- range(globaldata() %>% pull(col), na.rm = T)
      sliderInput(col, col, min = colrange[1], max = colrange[2], 
                  value = c(colrange[1], colrange[2]))
    })
  })
  
  output$whichcolumnsdelete <- renderUI({
    selectInput('whichcolumnsdelete', 'Choose columns to delete', choices = colnames(globaldata()),
                multiple = T, selected = NULL)
  })
  
  output$initGlobaldata <- renderText({
    getdata()
    print('')
  })
  
  ######################################################################################  VIEW DATA
  
  output$viewdata <- DT::renderDataTable({
    datatable(globaldata(), options = list(
      autowidth = F, scrollY = T, searching = T, searchHighlight = T, pageLength = 20
    ))
  })
  
  ######################################################################################  OVERVIEW
  
  output$infoboxOriginalSize <- renderInfoBox({
    infoBox('Size of original data', 
            HTML(paste0(dim(getdataOriginal())[1], ' rows<br>', 
                        dim(getdataOriginal())[2], ' columns')),
            color = 'purple', width = 3)
  })
  
  output$infoboxSize <- renderInfoBox({
    infoBox('Size of filtered data', 
            HTML(paste0(dim(globaldata())[1], ' rows<br>', 
                        dim(globaldata())[2], ' columns')),
            color = 'olive', width = 3)
  })
  
  output$infoboxNumberfilters <- renderInfoBox({
    infoBox('Active filters', sum(unlist(filterlist(), use.names = F)), color = 'aqua', width = 3)
  })
  
  output$missingvalues <- renderPlotly({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    nas <- round(getnumbermissings() / dim(globaldata())[1], digits = 4)
    nas <- nas[nas > 0]
    validate(need(is_empty(nas) == F, 'No NAs in data.'))
    plot_ly(y = orderXfactors(names(nas), nas, decr = F), x = nas, type = 'bar', color = 'pink') %>%
      add_plotlayout() %>%
      layout(title = 'Percentage of missing data')
      
  })
  
  output$selectcolfordist <- renderUI({
    selectInput('selectcolfordist', label = 'Select column', choices = colnames(globaldata()))
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
    infoBox('Number of columns with NAs', numberNas, color = 'olive', width = 3)
  })
  
  output$selectNAcharactercolumns <- renderUI({
    validate(need(length(getcharnas()) > 0, 'No character columns with missing data. '))
    selectInput('selectNAcharactercolumns', 'Select columns to remove NAs', 
                choices = c('ALL COLUMNS', getcharnas()), multiple = T)
  })
  
  output$selectNAnumericcolumns <- renderUI({
    validate(need(length(getnumnas()) > 0, 'No numeric columns with missing data. '))
    selectInput('selectNAnumericcolumns', 'Select columns to remove NAs', 
                choices = c('ALL COLUMNS', getnumnas()), multiple = T)
  })
  
  ######################################################################################  DETAILED ANALYSIS
  
  output$selectColumn1plot1 <- renderUI({
    selectInput('selectColumn1plot1', 'Select 1st column', choices = colnames(globaldata()), 
                selected = NULL, width = NULL )
  })
  
  output$selectColumn2plot1 <- renderUI({
    selectInput('selectColumn2plot2', 'Select 2nd column', choices = colnames(globaldata()), 
                selected = NULL, width = NULL)
  })
  
  output$plotidea2 <- renderPlotly({

  })
  
  ######################################################################################  CORRELATIONS
 
  output$correlationplot <- renderPlotly({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    mat <- get_cormat(globaldata(), maxFactor = maxFactorsForCor, NAtoZero = T)
    showNotification(paste0('One hot encoding is used for character columns. If more than ', maxFactorsForCor,
                            ' factors in a column, than this column will not be considered.'),  
                     type = 'message', duration = 10)
    plot_ly(x = colnames(mat), y = colnames(mat), z = round(mat, digits = 4), 
            type = 'heatmap', colors = 'PiYG') %>% add_plotlayout() %>%
      layout(title = 'Correlations')
  })
  
  ######################################################################################  FEATURE SELECTION
  
  output$featureselectthreshold <- renderUI({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    sliderInput('featureselectthreshold', 'Select number of features to delete', 
                min = 1, max = dim(globaldata())[2] - 2,
                  value = round(dim(globaldata())[2] / 2))
  })
  
  output$featimpplot <- renderPlotly({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    validate(need(
      featimp_dict[[input$featureselectmethod]][[regclassif_dict[[input$learnchoosetask]]]] == T,
      paste0('For ', input$learnchoosetask, ' the selected method is not available.')
    ))
    table <- getfeatimptable()
    plot_ly(y = orderXfactors(table[, 1], table[, 3], decr = F), x = table[, 3], type = 'bar', color = 'pink') %>%
      add_plotlayout() %>%
      layout(title = 'Feature importance')
  })
  
  ######################################################################################  TEXT ANALYSIS
  
  output$selectcolumnfortextcloud <- renderUI({
    selectInput('selectcolumnfortextcloud', 'Select column for wordcloud',
                choices = get_colsoftype(globaldata(), 'character'),
                selected = NULL, width = 250)
  })
  
  output$textcloud <- renderWordcloud2({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    validate(need(length(get_colsoftype(globaldata(), 'character')) > 0, 
                  'No character columns in data.'))
    wordtab <- get_wordfreq(globaldata() %>% pull(input$selectcolumnfortextcloud))
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
    selectInput('learnchoosetarget', 'Choose target variable', choices = colnames(globaldata()))
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

  ######################################################################################  COMPARE MODELS 
  
  # output$modelcomparisonplot <- renderPlotly({
  #   validate(need(length(models) > 0, 'No models trained yet.'))
  #   
  #   plot_ly(x = 1:length(models), y = )
  # })
  
  ######################################################################################  DOCUMENTATION
  
  output$inc <- renderUI({
    getPage('Documentation.html')
  })

  ###################################################################################### THE END.
  ###################################################################################### THE END.
  
})


