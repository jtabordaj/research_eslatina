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

varNamesDepartments <- c("AMAZONAS", "ANTIOQUIA", "ARAUCA", "ATLANTICO", "BOLIVAR", "BOGOTA", "BOYACA", "CALDAS", 
    "CAQUETA", "CASANARE", "CAUCA", "CESAR", "CHOCO", "CORDOBA", "CUNDINAMARCA", "GUAINIA", "GUAVIARE", 
    "HUILA", "LA GUAJIRA", "MAGDALENA", "META", "NARINO", "NORTE DE SANTANDER", "PUTUMAYO", "QUINDIO", 
    "RISARALDA", "SAN ANDRES", "SANTANDER", "SUCRE", "TOLIMA", "VALLE DEL CAUCA", 
    "VAUPES", "VICHADA"
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

bulkPush <- function(dept){
    subsetDF1 <- df %>% subset(Departamento == toupper(dept))
    municipalities <- subsetDF1 %>% distinct(Municipio)
    output <- data.frame()
    for (i in municipalities$Municipio) {
        subsetDF2 <- df %>% filter(Departamento == toupper(dept) & Municipio == toupper(i))
        mean_value <- mean(subsetDF2$totResidencial)
        row <- data.frame(Departamento = dept, Municipio = toupper(i), totResidencial = mean_value)
        output <- rbind(output, row)
        #output <<- rbind(output, row) %>% distinct()
  }
  return(output) 
}
