source("./reporte_estadistico/gas/dependencies.R")

# Gas Natural

data21 <- read_xlsx('./reporte_estadistico/data/gas/2021_fulldb.xlsx')
data22 <- read_xlsx('./reporte_estadistico/data/gas/2022_fulldb.xlsx')

spliceVariables(data21)
allDf <- ls()
dfList <- allDf[grep("^data_", allDf)]


#### INPUT ####

dfName <- "data_vcon"

#### INPUT ####
df <- get(dfName)

## This mutate is optional and is used to divide large values (e.g billions)
## Check once in a while

if(dfName == "data_fac"){
    df <- df %>% mutate(across(3:9, ~ ./1000000))
} else if(dfName == "data_pfac"){
    df <- df %>% mutate(across(3:9, ~ ./1000))
} else if(dfName == "data_tcon"){
    df <- df %>% mutate(across(3:9, ~ ./1000))
} else if(dfName == "data_vcon"){
    df <- df %>% mutate(across(3:9, ~ ./1000000))
}


###

initializeMetricsDF(df)
colnames(metricsDF) <- c("name")
metricsDF <- grabMetrics(df, metricsDF)
View(metricsDF)
summary(df)

doWrite <- TRUE
if(doWrite == TRUE){
    write_xlsx(metricsDF, paste("./reporte_estadistico/data/gas/metrics_",dfName,".xlsx", sep = ""))
} else {
   warning("Write signal is set to false")
}
