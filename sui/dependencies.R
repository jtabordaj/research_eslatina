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


varNamesDepartments <- c("AMAZONAS", "ANTIOQUIA", "ARAUCA", "ATLANTICO", "BOLIVAR", "BOGOTA", "BOGOTA, D.C.", "BOYACA", "CALDAS", 
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

readYearFiles <- function(year, type){
    var <- c("vcon", "tcon", "sus", "ptar", "pfac", "pcon", "fac")
    for(i in var){
    if(i != "pfac" && i != "pcon"){
        path <- paste("./sui/data/",type,"/",year,"/",i,"/",year,"_",i,".xls", sep = "")
        assign(paste(i,"_",year, sep=""), read_excel(path), envir=.GlobalEnv)
    } else {
        path <- paste("./sui/data/",type,"/",year,"/",i,"/",year,"_",i,".xlsx", sep = "")
        assign(paste(i,"_",year, sep=""), read_xlsx(path), envir=.GlobalEnv)
    }
}}

readOutputs <- function(year, type){
    var <- c("vcon", "tcon", "sus", "ptar", "pfac", "pcon", "fac")
    for(i in var){
        path <- paste("./sui/data/",type,"/",year,"/output/",i,".xlsx", sep = "")
        assign(paste(i,"_",year, sep=""), read_xlsx(path), envir=.GlobalEnv)
    }
}

readOutputs2 <- function(year, type){
    var <- c("sus", "tcon", "vconfac")
    for(i in var){
        path <- paste("./sui/data/",type,"/",year, "/", i,".xls", sep = "")
        assign(paste(i,"_",year, sep=""), read_excel(path), envir=.GlobalEnv)
    }
}

readCompFiles <- function(year, monthStart, monthEnd, type, variable){
    for(i in monthStart:monthEnd){
        path <- paste("./sui/data/",type,"/",year,"/",variable,"/compiled/compile",i,".xlsx", sep = "")
        assign(paste(variable,year,"_",i, sep=""), read_xlsx(path), envir=.GlobalEnv);
    }
}

formatNAs <- function(df){
    df[is.na(df)] <- 0
    return(df)
}

formatRegion <- function(df){
    df[, 1:4] <- lapply(df[, 1:4], as.factor)
    return(df)
}

formatNumbers <- function(df) {
  df[, 5:16] <- lapply(df[, 5:16], as.numeric)
  return(df)
}

formatNumbersYr <- function(df) {
  df[, 3:10] <- lapply(df[, 3:10], as.numeric)
  return(df)
}

formatNumbersYr2 <- function(df) {
  df[, 3:9] <- lapply(df[, 3:9], as.numeric)
  return(df)
}

formatRegionYr <- function(df){
    df[, 1:2] <- lapply(df[, 1:2], as.factor)
    return(df)
}

bulkPush <- function(dept){
    subsetDF1 <- df %>% subset(Departamento == toupper(dept))
    municipalities <- subsetDF1 %>% distinct(Municipio)
    output <- data.frame()
    for (i in municipalities$Municipio) {
        subsetDF2 <- df %>% filter(Departamento == toupper(dept) & Municipio == toupper(i))
        mean_value_E1 <- mean(subsetDF2$Estrato1, na.rm = TRUE)
        mean_value_E2 <- mean(subsetDF2$Estrato2, na.rm = TRUE)
        mean_value_E3 <- mean(subsetDF2$Estrato3, na.rm = TRUE)
        mean_value_E4 <- mean(subsetDF2$Estrato4, na.rm = TRUE)
        mean_value_E5 <- mean(subsetDF2$Estrato5, na.rm = TRUE)
        mean_value_E6 <- mean(subsetDF2$Estrato6, na.rm = TRUE)
        mean_value_R <- mean(subsetDF2$totResidencial, na.rm = TRUE)
        mean_value_NR <- mean(subsetDF2$totNoResidencial, na.rm = TRUE)
        row <- data.frame(Departamento = toupper(dept), 
            Municipio = toupper(i), 
            Estrato1 = mean_value_E1,
            Estrato2 = mean_value_E2,
            Estrato3 = mean_value_E3,
            Estrato4 = mean_value_E4,
            Estrato5 = mean_value_E5,
            Estrato6 = mean_value_E6,
            totResidencial = mean_value_R, 
            totNoResidencial = mean_value_NR)
        output <- rbind(output, row)
        #output <<- rbind(output, row) %>% distinct()
  }
  return(output) 
}

bulkPushCumSum <- function(dept){
    subsetDF1 <- df %>% subset(Departamento == toupper(dept))
    municipalities <- subsetDF1 %>% distinct(Municipio)
    output <- data.frame()
    for (i in municipalities$Municipio) {
        subsetDF2 <- df %>% filter(Departamento == toupper(dept) & Municipio == toupper(i))
        cumsum_value_E1 <- sum(subsetDF2$Estrato1, na.rm = TRUE)
        cumsum_value_E2 <- sum(subsetDF2$Estrato2, na.rm = TRUE)
        cumsum_value_E3 <- sum(subsetDF2$Estrato3, na.rm = TRUE)
        cumsum_value_E4 <- sum(subsetDF2$Estrato4, na.rm = TRUE)
        cumsum_value_E5 <- sum(subsetDF2$Estrato5, na.rm = TRUE)
        cumsum_value_E6 <- sum(subsetDF2$Estrato6, na.rm = TRUE)
        cumsum_value_R <- sum(subsetDF2$totResidencial, na.rm = TRUE)
        cumsum_value_NR <- sum(subsetDF2$totNoResidencial, na.rm = TRUE)
        row <- data.frame(Departamento = toupper(dept), 
            Municipio = toupper(i), 
            Estrato1 = cumsum_value_E1,
            Estrato2 = cumsum_value_E2,
            Estrato3 = cumsum_value_E3,
            Estrato4 = cumsum_value_E4,
            Estrato5 = cumsum_value_E5,
            Estrato6 = cumsum_value_E6,
            totResidencial = cumsum_value_R, 
            totNoResidencial = cumsum_value_NR)
        output <- rbind(output, row)
        #output <<- rbind(output, row) %>% distinct()
  }
  return(output) 
}

NaNSwitch <- function(x){
    do.call(cbind, lapply(x, is.nan))
}
