library(tidyverse)
library(ggplot2)
library(dplyr)
library(plyr)
library(readxl)
library(writexl)
library(haven)
library(purrr)
library(ggpubr)
library(moments)

varnames <- c("_fac", "_pcon", "_vcon", "_tcon", "_sus", "_ptar", "_pfac")

spliceVariables <- function(df){
    for(i in varnames){
        filtered <- grep(paste(i,"$", sep = ""), colnames(df), value = TRUE)
        assign(paste("data",i, sep = ""), cbind(df[,1:2], df[, filtered]) , envir=.GlobalEnv)
    }
}