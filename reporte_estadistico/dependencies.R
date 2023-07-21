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

createMetrics <- function(df, metric) {
    results <- vector("character", length = ncol(df) - 2)
    output <- data.frame()
    for (i in 3:10) {
        if (tolower(metric) == "mean") {
            calculatedMetric <- mean(df[, i], na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], metric = calculatedMetric)
        } else if (tolower(metric) == "variance") {
            calculatedMetric <- var(df[, i], na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], metric = calculatedMetric)
        } else if (tolower(metric) == "standard_deviation") {
            calculatedMetric <- sd(df[, i], na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], metric = calculatedMetric)
        } else if (tolower(metric) == "median") {
            calculatedMetric <- median(df[, i], na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], metric = calculatedMetric)
        } else if(tolower(metric) == "kurtosis"){
            lowerCutoff <- quantile(df[,i], probs = 10/100)
            upperCutoff <- quantile(df[,i], probs = 1 - 10/100)
            trimmed_column <- df[df[, i] >= lowerCutoff & df[, i] <= upperCutoff, i]
            calculatedMetric <- kurtosis(trimmed_column, na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], metric = calculatedMetric)
        } else if(tolower(metric) == "skewness"){
            lowerCutoff <- quantile(df[,i], probs = 10/100)
            upperCutoff <- quantile(df[,i], probs = 1 - 10/100)
            trimmed_column <- df[df[, i] >= lowerCutoff & df[, i] <= upperCutoff, i]
            calculatedMetric <- kurtosis(trimmed_column, na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], metric = calculatedMetric)
        } else if(tolower(metric) == "quantiles"){
            calculatedMetric <- quantile(df[, i], na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], "in order from 0 to 100%:", calculatedMetric[1], calculatedMetric[2], calculatedMetric[3],calculatedMetric[4], metric0 = calculatedMetric[5], sep = " ")
            row <- data.frame(name = colnames(df)[i], metric0 = calculatedMetric[1], metric25 = calculatedMetric[2], metric50 = calculatedMetric[3], metric75 = calculatedMetric[4], metric100 = calculatedMetric[5])
        }
        output <- rbind(output, row)
    }
    return(list(results = results, output = output))
}

## For kurtosis and skewness, the function drops lower 10 and upper 10