# 
# numcols = get_colsoftype(house, c('numeric', 'integer'))
# charcols = get_colsoftype(house, 'character')
# 
# plot_ly(x = house %>% pull(numcols[1]), y = house %>% pull(numcols[2]), type = 'scatter', mode = 'markers',
#         color = house$MSZoning)
# 
# plot_ly(x = house$Street, type = 'histogram')
# 
# 
# task = makeRegrTask('regtask', house, target = 'SalePrice')
# 
# rename_function <- function(namevec, renameList) {
#   new_names <- rep('', length(namevec))
#   for (i in 1:length(namevec)){
#     where <- match(namevec[i], names(renameList))
#     if (is.na(where)) {
#       new_names[i] <- namevec[i]
#     } else {
#       new_names[i] <- renameList[[where]]
#     }
#   }
#   return(new_names)
# }
# 
# names = colnames(house)
# newnames = make.names(colnames(house))
# rndict = c(names)
# names(rndict) = newnames
# 
# colnames(house) = make.names(colnames(house))
# charcols <- get_colsoftype(house, 'character')
# house[charcols] <- lapply(house[charcols] , factor)
# house1 = createDummyFeatures(house)
# task = makeRegrTask('regtask', house1, target = 'SalePrice')
# learner = makeLearner(cl = 'regr.rpart')
# model = train(learner = learner, task = task)

titanic = read_csv('/home/nicolas/Escritorio/Kaggle and other data/Titanic/train.csv') 
titanic %<>% select(-Ticket, -Name, -PassengerId) %>% 
  make_conformColnames() %>% make_strToFactors() %>% createDummyFeatures()
task = makeClassifTask(data = titanic, target = 'Survived')
params = algos_dict[['rpart']]$parameter
params$minbucket = 1
params$maxdepth = 1
params = params[!unlist(lapply(params, is.null))]
learner = makeLearner(cl = 'classif.rpart', par.vals = params)
desc = makeResampleDesc(method = 'CV', iter = 4)

model = resample(learner = learner, task = task, resampling = desc, 
                 measures = lapply(list("acc", "acc"), function(str){eval(parse(text = str))}))

preds = predict(model, newdata = titanic)
plot_ly(x = preds$data$truth, y = preds$data$response, type = 'scatter', mode = 'markers') %>% 
  add_plotlayout() %>% layout(xaxis = list(title = 'Target'), yaxis = list(title = 'Prediction'))

t = preds$data %>% group_by(response, truth) %>% summarize(n = n())
plot_ly(x = t$response, y = t$truth, z = t$n, type = 'heatmap', colors = 'PiYG')
s