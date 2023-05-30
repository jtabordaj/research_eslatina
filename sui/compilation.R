source("./sui/dependencies.R")

readFiles(2021, 1, 12, "energia", "pcon")
allDataFrames <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))

dflist <- list()
for(df in allDataFrames){
    dflist[[df]] <- get(df)
}

dflist <- lapply(dflist, formatNAs)
dflist <- lapply(dflist, formatNumbers)

summary(dflist[[1]])
