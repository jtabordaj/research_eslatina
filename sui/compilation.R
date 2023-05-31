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

##

muni <- df %>% distinct(Departamento, Municipio)
muni <- muni %>% mutate(totResidencial = NA)

testFunctionMean <- function(dept, munic){
    subsetDF <- df %>% subset(Departamento == toupper(dept) & Municipio == toupper(munic))
    muni <<- muni %>% mutate(totResidencial = ifelse(Municipio == toupper(munic), mean(subsetDF$totResidencial), totResidencial))
}

bulkPush <- function(dept){
    subsetDF1 <- df %>% subset(Departamento == toupper("ANTIOQUIA"))
    municipalities <- subsetDF1 %>% distinct(Municipio)
    for(i in municipalities){
        testFunctionMean(toupper(dept), i)
    }
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
