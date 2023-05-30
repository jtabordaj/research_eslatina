source("./sui/dependencies.R")

readFiles(2021, 1, 12, "energia", "pcon")
allDataFrames <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))

dflist <- list()
for(df in allDataFrames){
    dflist[[df]] <- get(df)
}
dflist <- lapply(dflist, formatNAs)
dflist <- lapply(dflist, formatNumbers)


output_text <- data.frame(matrix(ncol = 16, nrow = nrow(dflist[[1]])))
output_text[, 1:4] <- dflist[[1]][, 1:4]

output_numeric <- data.frame(matrix(ncol = 12, nrow = nrow(dflist[[1]])))

## Iterator

for (i in seq_along(dflist)) {
  aligned_rows <- match(rownames([dflist[[1]]]), rownames(dflist[[i]]))
  output_numeric <- output_numeric + dflist[[i]][aligned_rows, 5:16]
}
output_numeric <- output_numeric / 12
output_text[, 5:16] <- output_numeric
output <- output_text

