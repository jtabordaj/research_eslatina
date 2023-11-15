source("./sui/dependencies.R")

year <- 2022
type <- "energia"
readOutputs(year, "energia")

merge_criteria <- c("Departamento", "Municipio")
frameNames <- c("vcon", "tcon", "sus", "ptar", "pfac")

df <- left_join(get(paste("fac_",year, sep = "")), get(paste("pcon_",year, sep = "")), by = merge_criteria)

for(i in frameNames){
    df <- left_join(df, get(paste(i,"_",year, sep = "")), by = merge_criteria )
}

View(df)
path <- paste("./sui/data/",type,"/",year,"/",year,"_fulldb.xlsx", sep = "")
write_xlsx(df, path)
paste("Wrote file")