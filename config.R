source('Helper.R')

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

plot_axisfontstyle <- list(color = 'white')
plot_axisstyle <- list(tickfont = plot_axisfontstyle)
plot_titlefont <- list(size = 14, color = 'white')

maxFactorsForCor <- 10