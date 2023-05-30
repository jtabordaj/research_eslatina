source("./sui/dependencies.R")

readFiles(2021, 1, 12, "energia", "pcon")
allDataFrames <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))

df <- pcon2021_1

colnames(df) <- varnames
df <- df %>% formatNumbers()
df <- df %>% formatRegion()

numeric_interval <- 11
df[, 5:16][is.na(df[, 5:16])] <- NA
df <- df[complete.cases(df[, numeric_interval]),]

ejemplo <- df %>% subset(Departamento == "ANTIOQUIA" & Municipio == "BELLO")
muni <- df %>% distinct(Departamento, Municipio)
muni <- muni %>% mutate(totResidencial == mean(ejemplo$totResidencial))
#

######
dflist <- list()
for(df in allDataFrames){
    dflist[[df]] <- get(df)
}

dflist <- lapply(dflist, formatNAs)
dflist <- lapply(dflist, formatNumbers)
