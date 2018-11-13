#source('config.R')

shinyServer(function(input, output, session) {
  
  ######################################################################################  ~ REACTIVE SECTION ~
  ######################################################################################  --------------------
  ######################################################################################
  
  ######################################################################################  DATA STUFF
  
  globaldata <- reactiveVal(NULL)                 # the whole working data table 
  trainingdata <- reactiveVal(NULL)               # the training data (subset of globaldata)
  testingdata <- reactiveVal(NULL)                # the testing data (subset of globaldata)
  unencodeddata <- reactiveVal(NULL)              # snapshot of data before OHE is done such that a reset is possible
  unimputeddata <- reactiveVal(NULL)              # snapshot of data before imputing is done such that a reset is possible
  unnormalizeddata <- reactiveVal(NULL)           # snapshot of data before normalizing is done such that a reset is possible
  models <- reactiveVal(list())                   # list of all trained models 
  filterlist <- reactiveVal(NULL)                 # list which memorizes all columns where a filter has been set

  getdataOriginal <- reactive({
      if (input$sourceoffile == 'Upload data from computer') {
        file <- input$datafile
        if (is.null(file)) {
          return(NULL) 
        }
        dat <- read_csv(file$datapath, col_types = cols()) %>% make_conformColnames()
      } else if (input$sourceoffile == 'Choose example data') {
        dat <- eval(parse(text = tolower(input$datafile)))
      }
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
    globaldata() %>% make_strToFactors() 
    generateFilterValuesData(task = gettask(), learner = getlearner(), 
                             method = featimp_dict[[input$featureselectmethod]]$name)$data
  })
  
  ######################################################################################  GRAPHICAL ANALYSIS STUFF
  
  factorColsWithNotTooMany <- reactive({
    charcols <- get_colsoftype(globaldata(), c('character', 'factor'))
    colsWithTooMany <- delete_colsWithManyFactors(globaldata(), onlyNames = T)
    setdiff(charcols, colsWithTooMany)
  })
  
  ######################################################################################  MODEL STUFF
  
  gettaskPrefix <- reactive({
    case_when(input$learnchoosetask == 'Regression' ~ 'regr.',
              input$learnchoosetask == 'Classification' ~ 'classif.')
  })
  
  getalgo <- reactive({
    algo <- learningalgos_dict[[input$learnchoosealgo]]
    paste0(gettaskPrefix(), algo)
  })
  
  gettask <- reactive({
    
    # split data into train and test set 
    dat <- globaldata() %>% make_strToFactors() 
    trainTestSplit <- round((1 - input$percentageoftestingdata) * nrow(dat))
    dataForTraining <- dat[1:trainTestSplit, ]
    dataForTesting <- dat[(trainTestSplit + 1):nrow(dat), ]
    if (input$learnchoosetask == 'Classification') {
      validate(need(
        length(unique(dataForTraining %>% pull(input$learnchoosetarget))) > 1, 
        'There is only one class in the training data set.'))
    }

    trainingdata(dataForTraining)
    testingdata(dataForTesting)
    
    #create task
    if (input$learnchoosetask == 'Regression') {
      task = makeRegrTask('task', data = as.data.frame(trainingdata()), target = input$learnchoosetarget)
    } else if (input$learnchoosetask == 'Classification') {
      task = makeClassifTask('task', data = as.data.frame(trainingdata()), target = input$learnchoosetarget)
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
                 paste0('The following parameters are in the wrong format: ',
                        paste(couldNotBeConverted, collapse = ', '))))
    
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
    
    # check if more than one column available
    validate(need(ncol(globaldata()) > 1, 'You need at least two columns in your data to learn a model.'))
    
    # shuffle data before first learning in order to avoid having ordered classes in data
    if (length(models()) < 1) {
      print('wird geshufflet')
      globaldata(globaldata()[sample(nrow(globaldata())),])
    }
    
    # get table from mlr with properties of learner
    learnerTable <- suppressWarnings(listLearners())
    
    # check if there are columns with too many factors
    colsWithTooMany <- delete_colsWithManyFactors(globaldata(), maxFactor = 30, onlyNames = T)
    validate(need(length(colsWithTooMany) < 1, paste0(
                    'The following columns have to many factors: ',
                    paste(colsWithTooMany, collapse = ', '))))
    
    
    # check whether NAs exist and if they can be handled by the algorithm
    if (mean(getnumbermissings()) != 0) {
      nasPossible <- learnerTable %>% 
        filter(short.name == learningalgos_dict[[input$learnchoosealgo]]) %>% pull(missings)
      validate(need(nasPossible == T, 'This algorithm cant handle NAs. Please remove them.'))
    }
    
    # check whether multiclass can be handled by the algorithm if there are more than two classes
    if (input$learnchoosetask == 'Classification') {
      if (length(unique(globaldata() %>% pull(input$learnchoosetarget))) > 2) {
        multiclassPossible <- learnerTable %>% 
          filter(short.name == learningalgos_dict[[input$learnchoosealgo]]) %>% pull(multiclass)
        validate(need(multiclassPossible == T, 'This algorithm cant handle multiclass problems.'))
      }
    }
    
    # check whether factors can be handled by the algorithm
    charfactorCols <- get_colsoftype(globaldata(), c('character', 'factor'))
    if (length(charfactorCols) > 0) {
      factorsPossible <- learnerTable %>% 
        filter(short.name == learningalgos_dict[[input$learnchoosealgo]]) %>% pull(factors)
      validate(need(factorsPossible == T, 'This algorithm cant handle factors. Please encode the data.'))
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
      resample <- resample(learner, task, desc, show.info = F,
                           measures = lapply(list(input$performancemeasures), 
                                             function(str){eval(parse(text = str))}))
    } else {
      resample <- list('measures.test' = NULL)
    }
    
    # save the list of actual parameters 
    paramNames <- names(algos_dict[[learningalgos_dict[[input$learnchoosealgo]]]]$parameter)
    params <- setNames(as.list(rep(0, length(paramNames))), paramNames)
    for (param in names(params)) {
      params[[param]] <- input[[param]]
    }
    
    # train a final model and add it to the modellist of trained models and return stuff
    model <- train(task = task, learner = learner)
    preds <- predict(model, newdata = as.data.frame(testingdata()))
    newModels <- models()
    newModels[[length(models()) + 1]] <- list('model' = model, 'resample' = resample$measures.test,
                                              'parameters' = params, 'predictions' = preds,
                                              'algorithm' = input$learnchoosealgo)
    models(newModels)
    list('model' = model, 'learner' = learner, 'task' = task, 'resample' = resample$measures.test)
  })
  
  ######################################################################################  ~ OBSERVE SECTION ~
  ######################################################################################  --------------------
  ######################################################################################
  
  ######################################################################################  SELECT AND FILTER STUFF
  
  observeEvent(input$applydelete, {
    data <- globaldata()
    if (is.null(input$whichcolumnsdelete) == F) {
      data %<>% select(-one_of(input$whichcolumnsdelete))
    }
    globaldata(data)
  })
  
  observeEvent(input$importexampledata, {
    getdataOriginal()
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
    globaldata(data)
  })
  
  observeEvent(input$resetall, {
    globaldata(getdataOriginal())
  })
  
  ######################################################################################  CLEAN DATA STUFF
  
  observeEvent(input$imputeNA, {
    unimputeddata(globaldata())
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
    globaldata(unimputeddata())
  })
  
  observeEvent(input$removeconstantfeatures, {
    globaldata(globaldata() %>% removeConstantFeatures())
  })
  
  observeEvent(input$normalizefeatures, {
    unnormalizeddata(globaldata())
    if (input$selectcolsfornormalize == 'ALL COLUMNS') {
      colsToUse <- setdiff(get_colsoftype(globaldata(), c('integer', 'numeric')), input$learnchoosetarget)
    } else {
      colsToUse <- input$selectcolsfornormalize
    }
    normData <- normalizeFeatures(globaldata(), target = input$learnchoosetarget, 
                                  method = normalizing_dict[[input$selectmethodfornormalize]],
                                  cols = colsToUse)
    globaldata(normData)
  })
  
  observeEvent(input$resetnormalizing, {
    globaldata(unnormalizeddata())
  })
  ######################################################################################  CORRELATION AND OHE STUFF
  
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
  
  observeEvent(input$deletehighcorr, {
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    mat <- abs(get_cormat(globaldata()))
    mat[lower.tri(mat, diag = T)] <- 0
    whereHighCorrFeatures <- apply(mat, 1, function(row){any(row > input$thresholdfordeletehighcorr)})
    whichColsDelete <- names(whereHighCorrFeatures[whereHighCorrFeatures])
    whichColsDelete <- setdiff(whichColsDelete, input$learnchoosetarget)
    globaldata(globaldata() %>% select(-one_of(whichColsDelete)))
  })
  
  ######################################################################################  FEATURE IMPORTANCE STUFF
  
  observeEvent(input$deletefeaturesbythresh, {
    featuresToDelete <- get_leastImportanceFeatures(getfeatimptable(), input$featureselectthreshold)
    globaldata(globaldata() %>% select(-one_of(featuresToDelete)))
  })
  
  ######################################################################################  MODEL COMPARE STUFF
  
  observeEvent(input$deletemodels, {
    models(list())
  })
  
  ######################################################################################  ~ OUTPUT SECTION ~ 
  ######################################################################################  -------------------
  ######################################################################################
  
  ######################################################################################  SELECT AND FILTER
  
  output$datafile <- renderUI({
    if (input$sourceoffile == 'Upload data from computer') {
      fileInput('datafile', 
                'Choose Data',
                multiple = FALSE,
                accept = c('text/csv',
                           'text/comma-separated-values,text/plain',
                             '.csv'))
    } else if(input$sourceoffile == 'Choose example data') {
      selectInput('datafile', 'Choose example data', choices = c('Iris', 'Faithful'))
    }
  })
  
  output$importexampledata <- renderUI({
    if (input$sourceoffile == 'Choose example data') {
      actionButton('importexampledata', 'Import example data')
    }
  })
  
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
    if(input$sourceoffile == 'Upload data from computer') {
      getdataOriginal()
    }
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
  
  output$selectcolsfornormalize <- renderUI({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    validate(need(nchar(input$learnchoosetarget) > 0, 'Please select a target column first.'))
    numCols <- setdiff(get_colsoftype(globaldata(), c('integer', 'numeric')), input$learnchoosetarget)
    selectInput('selectcolsfornormalize', 'Select Columns to normalize', multiple = T, 
                choices = c('ALL COLUMNS', numCols))
  })
  
  ######################################################################################  GRAPHICAL ANALYSIS
  
  output$plotsettings1 <- renderUI({
    if (input$selectgraphtype == 'Network') {
      selectInput('selectoriginfornetwork', 'Select Origin for Network', choices = factorColsWithNotTooMany())
    } else if (input$selectgraphtype == 'Scatter') {
      selectInput('selectcol1forscatter', 'Select x axis column', 
                  choices = get_colsoftype(globaldata(), c('numeric', 'integer')))
    }
  })
  
  output$plotsettings2 <- renderUI({
    if (input$selectgraphtype == 'Network') {
      selectInput('selecttargetfornetwork', 'Select Target for Network', choices = 
                    setdiff(factorColsWithNotTooMany(), input$selectoriginfornetwork))
    } else if (input$selectgraphtype == 'Scatter') {
      selectInput('selectcol2forscatter', 'Select y axis column', 
                  choices = setdiff(get_colsoftype(globaldata(), c('numeric', 'integer')), input$selectcol1forscatter))
    }
  })
  
  output$graphanaplot <- renderPlotly({
    if (input$selectgraphtype == 'Network') {
      validate(need(length(factorColsWithNotTooMany()) > 1, 'You need at least two factor columns to show a network.'))
      produce_simple_sankey(globaldata(), input$selectoriginfornetwork, input$selecttargetfornetwork) %>%
        layout(plot_bgcolor = 'black', paper_bgcolor = 'black', 
               title = paste0(input$selectoriginfornetwork, ' --> ', input$selecttargetfornetwork), 
               font = list(size = 10), margin = list(t = 50))
    } else if (input$selectgraphtype == 'Scatter') {
      validate(need(length(get_colsoftype(globaldata(), c('numeric', 'integer'))) > 1, 
                    'You need at least two numeric columns.'))
      plot_ly(x = globaldata() %>% pull(input$selectcol1forscatter), 
              y = globaldata() %>% pull(input$selectcol2forscatter),
              color = 'pink') %>% add_plotlayout() %>% 
        layout(xaxis = list(title = input$selectcol1forscatter), yaxis = list(title = input$selectcol2forscatter))
    }
  })
  
  ######################################################################################  CORRELATIONS AND OHE
 
  output$correlationplot <- renderPlotly({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    mat <- get_cormat(globaldata(), maxFactor = maxFactorsForCor, NAtoZero = T)
    plot_ly(x = colnames(mat), y = colnames(mat), z = round(mat, digits = 4), 
            type = 'heatmap', colors = 'PiYG') %>% add_plotlayout() %>%
      layout(title = 'Correlations')
  })
  
  ######################################################################################  FEATURE SELECTION
  
  output$featureselectthreshold <- renderUI({
    validate(need(nrow(globaldata()) > 0, 'Please select data.'))
    sliderInput('featureselectthreshold', 'Select number of features to delete', 
                min = 1, max = dim(globaldata())[2] - 2,
                  value = round(dim(globaldata())[2] / 2))
  })
  
  output$featimpplot <- renderPlotly({
    validate(need(nrow(globaldata()) > 0, 'Please select data.'))
    validate(need(is.null(input$learnchoosetarget) == F, 'Choose a target column in Learn model tab first.'))
    validate(need(
      featimp_dict[[input$featureselectmethod]][[regclassif_dict[[input$learnchoosetask]]]] == T,
      paste0('For ', input$learnchoosetask, ' the selected method is not available.')
    ))
    if (input$learnchoosetask == 'Regression') {
      validate(need(is.numeric(globaldata() %>% pull(input$learnchoosetarget)), 
                    'Target column for regression has to be numeric.'))
    }
    table <- getfeatimptable()
    plot_ly(y = orderXfactors(table[, 1], table[, 3], decr = F), x = table[, 3], type = 'bar', color = 'pink') %>%
      add_plotlayout() %>%
      layout(title = 'Feature importance')
  })
  
  ######################################################################################  TEXT ANALYSIS
  
  output$selectcolumnfortextcloud <- renderUI({
    selectInput('selectcolumnfortextcloud', 'Select column for wordcloud',
                choices = get_colsoftype(globaldata(), c('character', 'factor')),
                selected = NULL, width = 250)
  })
  
  output$textcloud <- renderWordcloud2({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    validate(need(length(get_colsoftype(globaldata(), c('character', 'factor'))) > 0, 
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
                    choices = c('mse', 'rsq'), multiple = F, selected = 'rsq')
      } else if (input$learnchoosetask == 'Classification') {
        selectInput('performancemeasures', 'Performance measures', 
                    choices = c('acc'), selected = 'acc', multiple = F)
      } 
    }
  })
  
  output$plotperformanceoverview <- renderPlotly({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    if (input$showtrainortestperformance == 'Performance on training data') {
      preds <- predict(learnmodel()$model, newdata = as.data.frame(trainingdata()))
    } else {
      preds <- predict(learnmodel()$model, newdata = as.data.frame(testingdata()))
    }
    if (input$learnchoosetask == 'Regression') {
      p <- plot_ly(x = preds$data$truth, y = preds$data$response, type = 'scatter', mode = 'markers',
              color = 'pink')
    } else if(input$learnchoosetask == 'Classification') {
      preds <- preds$data %>% group_by(response, truth) %>% summarize(n = n())
      p <- plot_ly(x = preds$response, y = preds$truth, z = preds$n, type = 'heatmap', colors = 'PiYG') 
    }
    p %<>% add_plotlayout() %>% layout(xaxis = list(title = 'Target'), yaxis = list(title = 'Prediction'),
                                       title = 'Predictive performance')
  })
  
  output$testperformancetable <- renderTable({
    validate(need(is.null(input$datafile) == F, 'Please select data.'))
    validate(need(is.null(learnmodel()$resample) == F, 'No validation selected.'))
    learnmodel()$resample
  })

  ######################################################################################  COMPARE MODELS 
  
  output$selectmodelfordescription <- renderUI({
    if (length(models()) > 0) {
      selectInput('selectmodelfordescription', 'Select model', 
                  choices = as.character(1:length(models())), selected = '1')
    }
  })
  
  output$modeldescription <- renderText({
    if (length(models()) > 0) {
      selectedModel <- models()[[as.numeric(input$selectmodelfordescription)]]
      parameters <- selectedModel$parameters
      printStr <- paste0('Algorithm: ', selectedModel$algorithm, '<br>
                         ---------------------<br>Parameters:')
      for (param in names(parameters)) {
        printStr <- paste0(printStr, '<br>', param, ': ', parameters[[param]])
      }
      printStr <- paste0(printStr, '<br>---------------------<br>Features:<br>', 
                         paste(selectedModel$model$features, collapse = ', '))
      HTML(printStr)
    }
  })
  
  output$modelcomparisonplot <- renderPlotly({
    validate(need(length(models()) > 0, 'No models trained yet.'))
    performanceOfModels <- rep(0, length(models())) 
    for (i in 1:length(models())) {
      preds <- models()[[i]]$predictions
      performanceOfModels[i] <- performance(preds, measures = lapply(list(input$performancemeasures), 
                                                                     function(str){eval(parse(text = str))}))
    }
    plot_ly(x = 1:length(models()), y = performanceOfModels, type = 'bar', color = 'pink') %>% 
      add_plotlayout() %>% layout(title = 'Comparison of trained models', xaxis = list(title = 'Model'), 
                                  yaxis = list(title = paste0(input$performancemeasures, ' on test data')))
  })
  
  ###################################################################################### ~ THE END. ~ 
  ###################################################################################### --------------
  
})


