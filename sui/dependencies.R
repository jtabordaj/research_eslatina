library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)
library(readxl)
library(writexl)
library(haven)
library(purrr)

varnames <- c("Departamento", "Municipio", "Empresa", "Variable", 
    "Estrato1", "Estrato2", "Estrato3", "Estrato4", "Estrato5", "Estrato6",
    "totResidencial", "Industrial", "Comercial", "Oficial", "Otros", "totNoResidencial"
)

readFiles <- function(year, monthStart, monthEnd, type, variable){
    for(i in monthStart:monthEnd){
        path <- paste("./sui/data/",type,"/",year,"/",variable,"/",i,".csv", sep = "")
        assign(paste(variable,year,"_",i, sep=""), read_csv(path), envir=.GlobalEnv);
    }
}

formatNAs <- function(df){
    df[is.na(df)] <- 0
    return(df)
}

formatNumbers <- function(df) {
  df[, 5:16] <- lapply(df[, 5:16], as.numeric)
  return(df)
}

formatRegion <- function(df){
    df[, 1:4] <- lapply(df[, 1:4], as.factor)
    return(df)
}

mergeAndSum <- function(x, y){
    merged <- merge(x, y, by = c("row_names", "col_names"), all = TRUE)
    merged[, -c(1, 2)] <- rowSums(merged[, -c(1, 2)], na.rm = TRUE)
    return(merged)
}
