source("./reporte_estadistico/dependencies.R")

# Energia

data21 <- read_xlsx('./reporte_estadistico/data/energia/2021_fulldb.xlsx')
data22 <- read_xlsx('./reporte_estadistico/data/energia/2022_fulldb.xlsx')

spliceVariables(data21)
allDf <- ls()
dfList <- allDf[grep("^data_", allDf)]


#### INPUT ####

df <- data_fac

#### INPUT ####

## This mutate is optional and is used to divide large values (e.g billions)

df <- df %>% mutate(across(3:10, ~ ./1000000))

## Check once in a while

createMetrics(df, "mean")

initializeMetricsDF(df)
colnames(metricsDF) <- c("name")
metricsDF <- grabMetrics(df, metricsDF)




