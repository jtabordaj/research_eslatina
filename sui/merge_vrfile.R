source("./sui/dependencies.R")

year <- 2021
type <- "energia"

readYearFiles(year, type)

varnameEx <- "fac"
yr <- 2021

df <- get(paste(varnameEx, "_", yr, sep = ""))


###

df <- df %>% filter(Departamento %in% varNamesDepartments)
pattern <- "^MEDELL"
df <- df %>% mutate(Municipio = ifelse(grepl(pattern, Municipio), "MEDELLIN", Municipio))

df <- df %>% formatNumbersYr()
df <- df %>% formatRegionYr()
numeric_interval <- 9
df[, 3:10][is.na(df[, 3:10])] <- NA
df <- df[complete.cases(df[, numeric_interval]),]

##################### STOP ############################

if(varnameEx == "pfac" | varnameEx == "pcon" | varnameEx == "ptar"){
    for(i in unique(df$Departamento)){
    assign(paste("consolidado",i, sep = ""), bulkPush(i))
}} else {
   for(i in unique(df$Departamento)){
    assign(paste("consolidado",i, sep = ""), bulkPushCumSum(i))
}}

###

allDataFrames <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))
onlyConsolidado <- allDataFrames[grep("^consolidado", allDataFrames)]

dflist <- list()
for(df in onlyConsolidado){
    dflist[[df]] <- get(df)
}

df_combined <- do.call(rbind, dflist)
rownames(df_combined) <- c(1:nrow(df_combined))
df_combined[NaNSwitch(df_combined)] <- NA
for (i in 3:10) {
    colnames(df_combined)[i] <- paste0(colnames(df_combined)[i], "_", varnameEx, sep = "")
}

###

View(df_combined)
path <- paste("./sui/data/",type,"/",year,"/output/",varnameEx,".xlsx", sep = "")
write_xlsx(df_combined, path)
df_combined$Departamento %>% table()
paste("Wrote file")