
orderXfactors <- function(toOrder, orderBy, decr = T) {
  return(factor(toOrder, levels = unique(as.character(toOrder[order(orderBy, decreasing = decr)]))))
}

add_plotlayout <- function(p) {
  p %<>% layout(plot_bgcolor = "transparent", 
                paper_bgcolor = "transparent",
                font = plot_titlefont,
                xaxis = plot_axisstyle,
                yaxis = plot_axisstyle,
                margin = list(l = 40, r = 40, b = 40, t = 40))
  return(p)
}

get_colsoftype <- function(data, types) {
  colclasses <- sapply(data, class)
  return(names(colclasses[colclasses %in% types]))
}

get_wordfreq <- function(col) {
  rawtext <- paste(col, collapse = ' ')
  rawtext <- gsub('[[:punct:] ]+', ' ', rawtext)
  result <- data.frame(table(unlist(strsplit(rawtext, ' ')))) %>%
    arrange(desc(Freq))
  names(result) <- c('Word', 'Freq')
  return(result)
}

make_strToFactors <- function(data) {
  charcols <- get_colsoftype(data, 'character')
  data[charcols] <- lapply(data[charcols] , factor)
  return(data)
}

make_conformColnames <- function(data) {
  colnames(data) <- make.names(colnames(data))
  return(data)
}

delete_colsWithManyFactors <- function(data, maxFactor = 10, onlyNames = F) {
  datatmp <- data %>% select(one_of(get_colsoftype(data, c('character', 'factor'))))
  nuniques <- sapply(datatmp, function(col) {return(length(unique(col)))})
  todelete <- names(nuniques)[nuniques > maxFactor]
  if (onlyNames) {
    return (todelete)
  }
  return(data %>% select(-one_of(todelete)))
}

get_cormat <- function(data, maxFactor = 10, NAtoZero = T) {
  data <- delete_colsWithManyFactors(data, maxFactor) %>% make_strToFactors()
  dummies <- createDummyFeatures(data)
  cormatrix <- cor(dummies)
  if (NAtoZero) {
    cormatrix[is.na(cormatrix)] <- 0
  } 
  return(cormatrix)
}

getPage<-function(name) {
  return(includeHTML(name))
}

get_leastImportanceFeatures <- function(featimptable, number) {
  if (number >= nrow(featimptable)) {
    warning('You cant delete more features than are existing in the original data.')
  } 
  featimptable %<>% arrange_(names(featimptable)[3])
  return (featimptable$name[1:number])
}

produce_simple_sankey <- function(data, originCol, targetCol) {
  
  tmp <- data %>% group_by_(.dots = c(originCol, targetCol)) %>% summarise(count = n()) %>% arrange(desc(count))
  tmp <- tmp[complete.cases(tmp), ]
  labels <- c(unique(pull(tmp[, originCol])), unique(pull(tmp[, targetCol])))
  sources <- rep(0, nrow(tmp))
  targets <- rep(0, nrow(tmp))
  values <- rep(0, nrow(tmp))
  for(i in 1:nrow(tmp)) {
    sources[i] <- which(labels == pull(tmp[i, originCol]))[1] - 1
    targets[i] <- which(labels == pull(tmp[i, targetCol]))[1] - 1
    values[i] <- tmp$count[i]
  }
  p <- plot_ly(type = "sankey", orientation = "h",
               node = list(
                 label = labels,
                 pad = 10,
                 thickness = 20,
                 line = list(
                   color = "black",
                   width = 0.5
                 )
               ),
               link = list(source = sources, target = targets, value =  values),
               arrangement = "snap"
  )
  return(p)
}
