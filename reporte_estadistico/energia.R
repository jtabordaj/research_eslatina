source("./reporte_estadistico/dependencies.R")

# Energia

data21 <- read_xlsx('./reporte_estadistico/data/energia/2021_fulldb.xlsx')
data22 <- read_xlsx('./reporte_estadistico/data/energia/2022_fulldb.xlsx')

spliceVariables(data21)

createMetrics <- function(df){
    for(i in colnames(df)){
        mean(df$i)
    }
}

createMetrics(data_fac)

mean(data_fac$Estrato5_fac)
