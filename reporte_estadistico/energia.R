source("./reporte_estadistico/dependencies.R")

# Energia

data21 <- read_xlsx('./reporte_estadistico/data/energia/2021_fulldb.xlsx')
data22 <- read_xlsx('./reporte_estadistico/data/energia/2022_fulldb.xlsx')

spliceVariables(data21)
createMetrics(data_fac, "quantiles")$output

