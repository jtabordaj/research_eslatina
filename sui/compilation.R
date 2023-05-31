source("./sui/dependencies.R")

readFiles(2021, 1, 12, "energia", "pcon")
allDataFrames <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))

df <- pcon2021_1

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

##

