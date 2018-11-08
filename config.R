
######################################################################################  USED SOURCE FILES 

source('Helper.R')

######################################################################################  USED LIBRARIES

library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(plotly)
library(magrittr)
library(wordcloud2)
library(mlr)
library(RColorBrewer)
library(na.tools)
library(tidyimpute)
library(DT)
library(dict)
library(shinyjs)

######################################################################################  GENERAL SETTINGS

maxFactorsForCor <- 10

######################################################################################  PLOT SETTINGS

plot_axisfontstyle <- list(color = 'white')
plot_axisstyle <- list(tickfont = plot_axisfontstyle)
plot_titlefont <- list(size = 14, color = 'white')

######################################################################################  MLR TRANSLATIONS

learningalgos_dict <- dict()
learningalgos_dict[['Linear Regression']] <- 'lm'
learningalgos_dict[['Decision tree']] <- 'rpart'
learningalgos_dict[['Logistic Regression']] <- 'logreg'

featimp_dict <- dict()
featimp_dict[['Random Forest']] <- list(name = 'randomForest.importance', regr = F, classif = T)
featimp_dict[['Anova Test']] <- list(name = 'anova.test', regr = F, classif = T)
featimp_dict[['Correlation with target']] <- list(name = 'rank.correlation', regr = T, classif = F)

regclassif_dict <- dict()
regclassif_dict[['Regression']] <- 'regr'
regclassif_dict[['Classification']] <- 'classif'

normalizing_dict <- dict()
normalizing_dict[['Map to 0-1 interval']] <- 'range'
normalizing_dict[['Standardize']] <- 'standardize'
normalizing_dict[['Center']] <- 'center'
normalizing_dict[['Scale']] <- 'scale'


######################################################################################  ALGORITHMS

algos_dict <- dict()
algos_dict[['lm']] <- list(
  'parameter' = list()
)
algos_dict[['rpart']] <- list(
  'parameter' = list('minsplit' = NULL, 'minbucket' = NULL, 'maxdepth' = NULL, cp = NULL)
)
algos_dict[['logreg']] <- list(
  'parameter' = list()
)



