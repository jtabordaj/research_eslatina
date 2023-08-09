source("./reporte_estadistico/gas/dependencies.R")
source("./reporte_estadistico/gas/categoriesGas.R")

# varnames <- c("_fac", "_pcon", "_vcon", "_tcon", "_sus", "_ptar", "_pfac")
# sus tcon vcon 
# fac esta solo consolidado por total residencial

dfName <- "data_vcon"
dfSuffix <- "_vcon"

if(dfSuffix == "_vcon"){
    xlab <- "Valor consumo total de M3"
} else if(dfSuffix == "_tcon"){
    xlab <- "Total consumo M3"
} else if(dfSuffix == "_sus"){
    xlab <- "Suscriptores"
} else if(dfSuffix == "_fac"){
    xlab <- "Total Facturado"
} else if(dfSuffix == "_pcon"){
    xlab <- "Consumo Promedio"
} else if(dfSuffix == "_pfac"){
    xlab <- "Valor Factura"
} else if(dfSuffix == "_ptar"){
    xlab <- "Valor M3"
}

#### INPUT ####

df <- get(dfName)
if(dfName != "data_sus"){
    df <- merge(df, dataOnlyCat, by = c("Departamento", "Municipio"))
}
if(dfName == "data_fac"){
    df <- df %>% mutate(across(3:10, ~ ./1000000))
} else if(dfName == "data_vcon"){
    df <- df %>% mutate(across(3:10, ~ ./1000000))
}

initializeMetricsDF(df)
colnames(metricsDF) <- c("name")
metricsDF <- grabMetrics(df, metricsDF)
View(metricsDF)

doWrite <- TRUE
if(doWrite == TRUE){
    write_xlsx(metricsDF, paste("./reporte_estadistico/data/gas/metrics_",dfName,".xlsx", sep = ""))
} else {
   warning("Write signal is set to false")
}

###

output <- ggpDensities(df, 
    dfSuffix, 
    "", "", 
    xlab, "Densidad", 
    0.05, 0.95, "#4f0df7"
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



 