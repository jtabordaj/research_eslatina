source("./censo_dane/dependencies.R")

# Santa Lucia, Atlantico - 675
# Santa Rosa de Lima, Bolivar - 683

divipola <- 8
readFiles(divipola)
envir <- ls()
dfsEnvir <- envir[sapply(envir, function(x) is.data.frame(get(x)))]

# Cambios

for (i in dfsEnvir) {
  df <- get(i)
    df <- df[, !colnames(df) %in% "TIPO_REG", drop = FALSE]
    df <- df[, !colnames(df) %in% "UA_CLASE", drop = FALSE]
    assign(i, df, envir = .GlobalEnv)
}

divipolaFilter(divipola)

# DF Modules
source("./censo_dane/personas.R")
source("./censo_dane/vivienda.R")

# Join
data <- inner_join(vivienda, personas, by = c("COD_ENCUESTAS", "U_DPTO", "U_MPIO", "U_VIVIENDA"))

# Export
canWrite <- TRUE

if(canWrite == TRUE){
  wPath <- paste("./censo_dane/data/cnpv",divipola,".xlsx", sep = "")
  write_xlsx(data, wPath)
  message(paste("Wrote file at ", wPath, sep = ""))
}

