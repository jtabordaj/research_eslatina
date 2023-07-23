source("./reporte_estadistico/dependencies.R")

# Energia

data21 <- read_xlsx('./reporte_estadistico/data/energia/2021_fulldb.xlsx')
data22 <- read_xlsx('./reporte_estadistico/data/energia/2022_fulldb.xlsx')

spliceVariables(data21)
allDf <- ls()
dfList <- allDf[grep("^data_", allDf)]

createMetrics(data_fac, "mean")

initializeMetricsDF(data_fac)
colnames(metricsDF) <- c("name")
metricsDF <- grabMetrics(data_fac, metricsDF)




