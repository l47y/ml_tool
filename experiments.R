
paramSet = getParamSet("classif.rpart")$pars
params = names(algos_dict[['rpart']]$parameter)

l = list("cp" = 0.01, "minsplit" = 3.2, "minbucket" = "heinz", "maxdepth" = 2)

vec = list()
for (param in params) {
  typeRequired = paramSet[[param]]$type
  actualValue = l[[param]]
  conversion = paste0("as.", typeRequired)
  evalStr = paste0(conversion, "(actualValue)")
  vec[param] <- try(eval(parse(text = paste0(conversion, "(actualValue)"))))
}

names(vec[is.na(vec)])
