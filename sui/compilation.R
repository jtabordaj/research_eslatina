source("./sui/dependencies.R")

readFiles(2021, 1, 12, "energia", "pcon")
allDataFrames <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))

df <- pcon2021_1
colnames(df) <- varnames
pattern <- "^MEDELL"

df <- df %>% mutate(Municipio = ifelse(grepl(pattern, Municipio), "MEDELLIN", Municipio))
df <- df %>% formatNumbers()
df <- df %>% formatRegion()

numeric_interval <- 11
df[, 5:16][is.na(df[, 5:16])] <- NA
df <- df[complete.cases(df[, numeric_interval]),]

##

output <- df %>% distinct(Departamento, Municipio)
output <- output %>% mutate(totResidencial = NA)

bulkPush <- function(dept){
    subsetDF1 <- df %>% subset(Departamento == toupper(dept))
    municipalities <- subsetDF1 %>% distinct(Municipio)
    output <- data.frame()
    for (i in municipalities$Municipio) {
        subsetDF2 <- df %>% filter(Departamento == toupper(dept) & Municipio == toupper(i))
        mean_value <- mean(subsetDF2$totResidencial)
        row <- data.frame(Municipio = toupper(i), totResidencial = mean_value)
        output <- rbind(output, row)
  }
  return(output) 
}

bulkPush("ANTIOQUIA")

testFunctionMean("ANTIOQUIA", "ARGELIA")
testFunctionMean("ANTIOQUIA", "ABEJORRAL")
#

######
dflist <- list()
for(df in allDataFrames){
    dflist[[df]] <- get(df)
}

dflist <- lapply(dflist, formatNAs)
dflist <- lapply(dflist, formatNumbers)
