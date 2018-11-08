titanic = read_csv('/home/nicolas/Escritorio/RProjects/ml_tool/data/titanic.csv')
titanic %<>% delete_colsWithManyFactors() %>% createDummyFeatures() %>% make_strToFactors()
wherenas = apply(titanic, 1, function(row) {any(is.na(row))})
titanic = titanic[!wherenas, ]
task = makeClassifTask(id = 'task', titanic, target = 'Survived')
learner = makeLearner('classif.logreg', par.vals = list())
model = train(task = task, learner = learner)
