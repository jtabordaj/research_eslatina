source("./sui/dependencies.R")

year <- 2021
type <- "gas"
readOutputs2(year, type)

merge_criteria <- c("Departamento", "Municipio")
frameNames <- c("vconfac")

df <- left_join(get(paste("sus_",year, sep = "")), get(paste("tcon_",year, sep = "")), by = merge_criteria)

for(i in frameNames){
    df <- left_join(df, get(paste(i,"_",year, sep = "")), by = merge_criteria )
}

df[, 1:2] <- lapply(df[, 1:2], as.factor)


