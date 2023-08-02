source("./reporte_estadistico/gas/dependencies.R")
source("./reporte_estadistico/gas/metricsGas.R")

## One variable

output <- ggpDensities(df, "_vcon", "", "", "Valor consumo total de M3", "Densidad", 0.1, 0.9)

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
    margin = c(0.5, 0.5)
)
