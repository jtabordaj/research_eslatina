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
options(scipen = 999)

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
            calculatedMetric <- skewness(trimmed_column, na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], ":", calculatedMetric, sep = " ")
            row <- data.frame(name = colnames(df)[i], skewness = calculatedMetric)
        } else if(tolower(metric) == "quantiles"){
            calculatedMetric <- quantile(df[, i], na.rm = TRUE)
            results[i - 2] <- paste(metric, "value of", colnames(df)[i], "in order from 0 to 100%:", calculatedMetric[1], calculatedMetric[2], calculatedMetric[3],calculatedMetric[4], calculatedMetric[5], sep = " ")
            row <- data.frame(name = colnames(df)[i], quant0 = calculatedMetric[1], quant25 = calculatedMetric[2], quant50 = calculatedMetric[3], quant75 = calculatedMetric[4], quant100 = calculatedMetric[5])
        } else {
            stop("Check metric")
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

ggpDensity <- function(data, column, title, subtitle, xLab, yLab, lowerLim, upperLim){
    lowerLimit <- quantile(data[[column]], lowerLim, na.rm = TRUE)
    upperLimit <- quantile(data[[column]], upperLim, na.rm = TRUE) 
    c <- ggplot(df, aes_string(x = column))
    c + geom_density(kernel = "gaussian", color = "#4404a2", fill = "#4404a2") +
    ggtitle(title) +
    labs(
    title = title,
    subtitle = subtitle) +
    xlab(xLab) +
    ylab(yLab) +
    xlim(lowerLimit, upperLimit) +
    theme_light()+
    theme(
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14,),
        plot.title = element_text(size = 18, hjust = 0.5)
    ) 
}

ggpDensities <- function(data, var, title, subtitle, xLab, yLab, lowerLim, upperLim){
    E1 <- ggpDensity(data, paste('Estrato1',var, sep = ""), paste(title, "Estrato 1", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    E2 <- ggpDensity(data, paste('Estrato2',var, sep = ""), paste(title, "Estrato 2", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    E3 <- ggpDensity(data, paste('Estrato3',var, sep = ""), paste(title, "Estrato 3", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    E4 <- ggpDensity(data, paste('Estrato4',var, sep = ""), paste(title, "Estrato 4", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    E5 <- ggpDensity(data, paste('Estrato5',var, sep = ""), paste(title, "Estrato 5", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    E6 <- ggpDensity(data, paste('Estrato6',var, sep = ""), paste(title, "Estrato 6", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    totR <- ggpDensity(data, paste('totResidencial',var, sep = ""), paste(title, "Consumidores residenciales", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    totNR <- ggpDensity(data, paste('totNoResidencial',var, sep = ""), paste(title, "Consumidores no residenciales", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    return(list(E1 = E1, E2 = E2, E3 = E3, E4 = E4, E5 = E5, E6 = E6, RES = totR, NORES = totNR))
}

ggpBoxplot <- function(data, column, title, subtitle, xLab, yLab, lowerLim, upperLim){
    lowerLimit <- quantile(data[[column]], lowerLim, na.rm = TRUE)
    upperLimit <- quantile(data[[column]], upperLim, na.rm = TRUE) 
    c <- ggplot(df, aes_string(x = column))
    c + geom_boxplot(color = "#6600ff", fill = "#6600ff") + ggtitle(title) +
    labs(
    title = title,
    subtitle = subtitle) +
    xlab(xLab) +
    ylab(yLab) +
    xlim(lowerLimit, upperLimit) +
    theme_light()+
    theme(
        plot.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14,),
        plot.title = element_text(size = 18, hjust = 0.5)
    ) 
}

ggpBoxplots <- function(data, var, title, subtitle, xLab, yLab, lowerLim, upperLim){
    E1 <- ggpBoxplot(data, paste('Estrato1',var, sep = ""), paste(title, "Estrato 1", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    E2 <- ggpBoxplot(data, paste('Estrato2',var, sep = ""), paste(title, "Estrato 2", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    E3 <- ggpBoxplot(data, paste('Estrato3',var, sep = ""), paste(title, "Estrato 3", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    E4 <- ggpBoxplot(data, paste('Estrato4',var, sep = ""), paste(title, "Estrato 4", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    E5 <- ggpBoxplot(data, paste('Estrato5',var, sep = ""), paste(title, "Estrato 5", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    E6 <- ggpBoxplot(data, paste('Estrato6',var, sep = ""), paste(title, "Estrato 6", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    totR <- ggpBoxplot(data, paste('totResidencial',var, sep = ""), paste(title, "Consumidores residenciales", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    totNR <- ggpBoxplot(data, paste('totNoResidencial',var, sep = ""), paste(title, "Consumidores no residenciales", sep = " "), subtitle, xLab, yLab, lowerLim, upperLim)
    return(list(E1 = E1, E2 = E2, E3 = E3, E4 = E4, E5 = E5, E6 = E6, RES = totR, NORES = totNR))
}