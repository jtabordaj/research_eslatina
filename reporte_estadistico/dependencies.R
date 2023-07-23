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
            row <- data.frame(name = colnames(df)[i], mean = calculatedMetric)
        } else if (tolower(metric) == "variance") {
            calculatedMetric <- var(df[, i], na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], variance = calculatedMetric)
        } else if (tolower(metric) == "standard_deviation") {
            calculatedMetric <- sd(df[, i], na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], std_dev = calculatedMetric)
        } else if (tolower(metric) == "median") {
            calculatedMetric <- median(df[, i], na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], median = calculatedMetric)
        } else if(tolower(metric) == "kurtosis"){
            lowerCutoff <- quantile(df[,i], probs = 10/100, na.rm = TRUE)
            upperCutoff <- quantile(df[,i], probs = 1 - 10/100, na.rm = TRUE)
            trimmed_column <- df[df[, i] >= lowerCutoff & df[, i] <= upperCutoff, i]
            calculatedMetric <- kurtosis(trimmed_column, na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], kurtosis = calculatedMetric)
        } else if(tolower(metric) == "skewness"){
            lowerCutoff <- quantile(df[,i], probs = 10/100, na.rm = TRUE)
            upperCutoff <- quantile(df[,i], probs = 1 - 10/100, na.rm = TRUE)
            trimmed_column <- df[df[, i] >= lowerCutoff & df[, i] <= upperCutoff, i]
            calculatedMetric <- kurtosis(trimmed_column, na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], skewness = calculatedMetric)
        } else if(tolower(metric) == "quantiles"){
            calculatedMetric <- quantile(df[, i], na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], "in order from 0 to 100%:", calculatedMetric[1], calculatedMetric[2], calculatedMetric[3],calculatedMetric[4], metric0 = calculatedMetric[5], sep = " ")
            row <- data.frame(name = colnames(df)[i], quant0 = calculatedMetric[1], quant25 = calculatedMetric[2], quant50 = calculatedMetric[3], quant75 = calculatedMetric[4], quant100 = calculatedMetric[5])
        }
        output <- rbind(output, row)
    }
    return(list(results = results, output = output))
}

voi <- c("mean", "variance", "standard_deviation", "median", "kurtosis", "skewness", "quantiles")

initializeMetricsDF <- function(df) {
  rowNames <- data.frame(colnames(df[3:10]))
  allMetrics <- data.frame(names = rowNames)
  assign("metricsDF", allMetrics, envir = .GlobalEnv)
}

grabMetrics <- function(df, metricsDF) {
  for (i in voi) {
    obj <- createMetrics(df, i)$output
    metricsDF <- inner_join(obj, metricsDF, by = "name")
  }
  return(metricsDF)  
}

## For kurtosis and skewness, the function drops lower 10 and upper 10