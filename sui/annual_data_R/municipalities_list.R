
source("./sui/dependencies.R")

## Generate a db of regions (Basis of consumption is demand so I load subscribers)

data <- read_excel("./sui/data/energia/2021/sus/2021_sus.xls")

data$Departamento <- as.factor(data$Departamento)
data$Municipio <- as.factor(data$Municipio)

summary(data)

regions <- data[,1:2]

unique(regions$Departamento)

regions_unique <- regions[!duplicated(regions), ]

##### Load generated dbs to find regions without data

data21 <- read_xlsx("./sui/data/energia/2021/2021_fulldb.xlsx")
data22 <- read_xlsx("./sui/data/energia/2022/2022_fulldb.xlsx")

mergeCriteria <- c("Departamento", "Municipio")

data21_nonPresent <- anti_join(regions_unique, data21, by = mergeCriteria)
data22_nonPresent <- anti_join(regions_unique, data22, by = mergeCriteria)

data2122_nonPresent <- anti_join(data21, data22, by = mergeCriteria)

write_xlsx(data21_nonPresent, "./sui/data/energia/2021/2021_dbNotPresent.xlsx")
write_xlsx(data22_nonPresent, "./sui/data/energia/2022/2022_dbNotPresent.xlsx")
write_xlsx(data2122_nonPresent,"./sui/data/energia/2122_dbNotPresent.xlsx")
