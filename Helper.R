
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

get_cormat <- function(data, maxFactor = 10) {
  charcols <- get_colsoftype(data, 'character')
  datatmp <- data %>% select(one_of(charcols))
  nuniques <- sapply(datatmp, function(col) {return(length(unique(col)))})
  todelete <- names(nuniques)[nuniques > maxFactor]
  data %<>% select(-one_of(todelete)) %>% make_strToFactors()
  dummies <- createDummyFeatures(data)
  return(cor(dummies))
}

