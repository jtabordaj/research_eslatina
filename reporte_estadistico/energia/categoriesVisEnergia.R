source("./reporte_estadistico/energia/dependencies.R")
source("./reporte_estadistico/energia/categoriesEnergia.R")

# varnames <- c("_fac", "_pcon", "_vcon", "_tcon", "_sus", "_ptar", "_pfac")

dfName <- "data_vcon"
dfSuffix <- "_vcon"
xlab <- "Valor consumo total de kW/h"
#### INPUT ####

df <- get(dfName)
if(dfName != "data_sus"){
    df <- merge(df, dataOnlyCat, by = c("Departamento", "Municipio"))
}
if(dfName == "data_fac"){
    df <- df %>% mutate(across(3:10, ~ ./1000000))
} else if(dfName == "data_pfac"){
    df <- df %>% mutate(across(3:10, ~ ./1000))
} else if(dfName == "data_tcon"){
    df <- df %>% mutate(across(3:10, ~ ./1000))
} else if(dfName == "data_vcon"){
    df <- df %>% mutate(across(3:10, ~ ./1000000))
}

initializeMetricsDF(df)
colnames(metricsDF) <- c("name")
metricsDF <- grabMetrics(df, metricsDF)
View(metricsDF)
###

output <- ggpDensities(df, 
    dfSuffix, 
    "", "", 
    xlab, "Densidad", 
    0.01, 0.99, "#0080ff"
)

E1 <- output$E1
E2 <- output$E2
E3 <- output$E3
E4 <- output$E4
E5 <- output$E5
E6 <- output$E6

ggarrange(
    E1, E2, E3,
    E4, E5, E6,
    ncol = 2, nrow = 3,
    margin = c(0.5, 0.5))
summary(df)
