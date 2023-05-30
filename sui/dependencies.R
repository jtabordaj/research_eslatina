library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)
library(readxl)
library(writexl)
library(haven)
library(purrr)

readFiles <- function(year, monthStart, monthEnd, type, variable){
    for(i in monthStart:monthEnd){
        path <- paste("./sui/data/",type,"/",year,"/",variable,"/",i,".csv", sep = "")
        assign(paste(variable,year,"_",i, sep=""), read_csv(path), envir=.GlobalEnv);
    }
}

formatNAs <- function(df){
  df[df == "ND"] <- NA
  return(df)
}

formatNumbers <- function(df){
    df[, 5:16] <- lapply(df[, 5:16], as.numeric)
    return(df)
}

mergeAndSum <- function(x, y){
    merged <- merge(x, y, by = c("row_names", "col_names"), all = TRUE)
    merged[, -c(1, 2)] <- rowSums(merged[, -c(1, 2)], na.rm = TRUE)
    return(merged)
}
