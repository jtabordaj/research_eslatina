source("./sui/dependencies.R")

month <- 11
year <- 2021
type <- "energia"
variable <- "pcon"


######################################

readFiles(year, 1, 12, type, variable)
allDataFrames <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))

df <- get(paste("pcon2021_",month, sep = ""))

##

colnames(df) <- varnames
df <- df %>% filter(Departamento %in% varNamesDepartments)
pattern <- "^MEDELL"
df <- df %>% mutate(Municipio = ifelse(grepl(pattern, Municipio), "MEDELLIN", Municipio))
df <- df %>% formatNumbers()
df <- df %>% formatRegion()
numeric_interval <- 11
df[, 5:16][is.na(df[, 5:16])] <- NA
df <- df[complete.cases(df[, numeric_interval]),]

##

for(i in unique(df$Departamento)){
    assign(paste("consolidado",i, sep = ""), bulkPush(i))
}

allDataFrames <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))
onlyConsolidado <- allDataFrames[grep("^consolidado", allDataFrames)]

dflist <- list()
for(df in onlyConsolidado){
    dflist[[df]] <- get(df)
}

df_combined <- do.call(rbind, dflist)
rownames(df_combined) <- c(1:nrow(df_combined))

df_combined[NaNSwitch(df_combined)] <- NA
##

path <- paste("./sui/data/",type,"/",year,"/",variable,"/compiled/compile",month,".xlsx", sep = "")
df_combined$Departamento %>% table()
write_xlsx(df_combined, path)

######################################