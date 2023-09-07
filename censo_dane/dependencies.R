library(dplyr)
library(tidyverse)
library(ggplot2)
library(plyr)
library(readxl)
library(writexl)
library(haven)

path08 <- paste("./censo_dane/data/08", sep = "")
path13 <- paste("./censo_dane/data/13", sep = "")
fList08 <- list.files(path = path08)
fList13 <- list.files(path = path13)

readFiles <- function(divipola){
    if(divipola == 8){
        for(i in fList08){
            data <- read.csv(paste(path08, "/", i, sep = ""))
            df_name <- paste(gsub(".CSV", "", basename(i)), divipola, sep = "")
            assign(df_name, data, envir = .GlobalEnv)
        }
    } else if(divipola == 13){
            for(i in fList13){
            data <- read.csv(paste(path13, "/", i, sep = ""))
            df_name <- paste(gsub(".CSV", "", basename(i)), divipola, sep = "")
            assign(df_name, data, envir = .GlobalEnv)
        }
    } else {
       stop("Either Divipola of 08 or 13")
    }
}

divipolaFilter <- function(divipola){
    if(divipola == 8){
        for(i in dfsEnvir){
            df <- get(i)
            df <- df %>% filter(U_MPIO == 675)
            assign(i, df, envir = .GlobalEnv)
        }
    } else if(divipola == 13){
        for(i in dfsEnvir){
            df <- get(i)
            df <- df %>% filter(U_MPIO == 683)
            assign(i, df, envir = .GlobalEnv)
        }
    } else {
        stop("Either Divipola of 08 or 13")
    }
}

mergeCriteria <- c("U_DPTO", "U_MPIO", "UA_CLASE", "COD_ENCUESTAS", )