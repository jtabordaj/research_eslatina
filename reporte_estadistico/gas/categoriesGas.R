source("./reporte_estadistico/gas/dependencies.R")

# Energia

data21 <- read_xlsx('./reporte_estadistico/data/gas/2021_fulldb.xlsx')
data22 <- read_xlsx('./reporte_estadistico/data/gas/2022_fulldb.xlsx')

data <- data21
    quartiles <- data.frame(quart = quantile(data$totResidencial_sus, probs = c(0.2, 0.4, 0.6, 0.8)))
    data <- data %>% mutate(cat1_sus = ifelse(totResidencial_sus <= quartiles[1,1], 1, 0))
    data <- data %>% mutate(cat2_sus = ifelse(totResidencial_sus > quartiles[1,1] & totResidencial_sus <= quartiles[2,1], 1, 0))
    data <- data %>% mutate(cat3_sus = ifelse(totResidencial_sus > quartiles[2,1] & totResidencial_sus <= quartiles[3,1], 1, 0))
    data <- data %>% mutate(cat4_sus = ifelse(totResidencial_sus > quartiles[3,1] & totResidencial_sus <= quartiles[4,1], 1, 0))
    data <- data %>% mutate(cat5_sus = ifelse(totResidencial_sus > quartiles[4,1], 1, 0))
#
dataOnlyCat <- data %>% select("Departamento", "Municipio", "cat1_sus", "cat2_sus", "cat3_sus", "cat4_sus", "cat5_sus")

subset1 <- data %>% filter(cat1_sus != 0)
subset2 <- data %>% filter(cat2_sus != 0)
subset3 <- data %>% filter(cat3_sus != 0)
subset4 <- data %>% filter(cat4_sus != 0)
subset5 <- data %>% filter(cat5_sus != 0)

allDf <- ls()
dfList <- allDf[grep("^data_", allDf)]

#### INPUT ####

spliceVariables(subset5)