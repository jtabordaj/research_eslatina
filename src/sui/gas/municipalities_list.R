
source("./sui/dependencies.R")

## Generate a db of regions (Basis of consumption is demand so I load subscribers)

data <- read_excel("./sui/data/energia/2021/sus/2021_sus.xls")

data$Departamento <- as.factor(data$Departamento)
data$Municipio <- as.factor(data$Municipio)
regions <- data[,1:2]
regions_unique <- regions[!duplicated(regions), ]


##### Load generated dbs to find regions without data

mergeCriteria <- c("Departamento", "Municipio")

data21 <- read_xlsx("./sui/data/gas/2021_fulldb.xlsx")
data22 <- read_xlsx("./sui/data/gas/2022_fulldb.xlsx")
data2122_nonPresent <- anti_join(data22, data21, by = mergeCriteria)

write_xlsx(data2122_nonPresent,"./sui/data/gas/2122_dbNotPresent.xlsx")

#############

data21_nonPresent <- anti_join(regions_unique, data21, by = mergeCriteria)
data22_nonPresent <- anti_join(regions_unique, data22, by = mergeCriteria)

write_xlsx(data21_nonPresent, "./sui/data/gas/2021/2021_dbNotPresent.xlsx")
write_xlsx(data22_nonPresent, "./sui/data/gas/2022/2022_dbNotPresent.xlsx")
