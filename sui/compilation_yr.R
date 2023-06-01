source("./sui/dependencies.R")

year <- 2022
type <- "energia"
variable <- "pcon"


######################################

readCompFiles(year, 1, 12, type, variable)
merge_criteria <- c("Departamento", "Municipio")
df <- left_join(get(paste(variable,year,"_1", sep = "")), get(paste(variable,year,"_2", sep = "")), by = merge_criteria )

for(i in 3:12){
    df <- left_join(df, get(paste(variable,year,"_",i, sep = "")), by = merge_criteria )
}

patternE1 <- paste("^Estrato1", sep = "")
patternE2 <- paste("^Estrato2", sep = "")
patternE3 <- paste("^Estrato3", sep = "")
patternE4 <- paste("^Estrato4", sep = "")
patternE5 <- paste("^Estrato5", sep = "")
patternE6 <- paste("^Estrato6", sep = "")

E1 <- grep(patternE1, names(df), value = TRUE)
E2 <- grep(patternE2, names(df), value = TRUE)
E3 <- grep(patternE3, names(df), value = TRUE)
E4 <- grep(patternE4, names(df), value = TRUE)
E5 <- grep(patternE5, names(df), value = TRUE)
E6 <- grep(patternE6, names(df), value = TRUE)

df <- df %>% mutate(Estrato1 = rowMeans(df[, E1], na.rm = TRUE))
df <- df %>% mutate(Estrato2 = rowMeans(df[, E2], na.rm = TRUE))
df <- df %>% mutate(Estrato3 = rowMeans(df[, E3], na.rm = TRUE))
df <- df %>% mutate(Estrato4 = rowMeans(df[, E4], na.rm = TRUE))
df <- df %>% mutate(Estrato5 = rowMeans(df[, E5], na.rm = TRUE))
df <- df %>% mutate(Estrato6 = rowMeans(df[, E6], na.rm = TRUE))

patternTotRes <- paste("^totResidencial", sep = "")
tRes <- grep(patternTotRes, names(df), value = TRUE)

patternTotNoRes <- paste("^totNoResidencial", sep = "")
tNoRes <- grep(patternTotNoRes, names(df), value = TRUE)

df <- df %>% mutate(totResidencial = rowMeans(df[, tRes], na.rm = TRUE))
df <- df %>% mutate(totNoResidencial = rowMeans(df[, tNoRes], na.rm = TRUE))

df <- df %>% select(any_of(varnames))

df[NaNSwitch(df)] <- NA

###

path <- paste("./sui/data/",type,"/",year,"/",variable,"/",year,".xlsx", sep = "")
write_xlsx(df, path)