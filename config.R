
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
featimp_dict[['Random Forest']] <- 'randomForestSRC.rfsrc'
featimp_dict[['Anova Test']] <- 'anova.test'
featimp_dict[['Random Permutation']] <- 'permutation.importance'
featimp_dict[['Correlation with target']] <- 'rank.correlation'

######################################################################################  ALGORITHMS

algos_dict <- dict()
algos_dict[['lm']] <- list(
  'parameter' = list()
)
algos_dict[['rpart']] <- list(
  'parameter' = list('minsplit' = NULL, 'minbucket' = NULL, 'maxdepth' = NULL)
)
algos_dict[['logreg']] <- list(
  'parameter' = list()
)


