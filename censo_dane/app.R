source("./censo_dane/dependencies.R")

# Santa Lucia, Atlantico - 675
# Santa Rosa de Lima, Bolivar - 683

divipola <- 8
readFiles(divipola)
envir <- ls()
dfsEnvir <- envir[sapply(envir, function(x) is.data.frame(get(x)))]

divipolaFilter(divipola)




