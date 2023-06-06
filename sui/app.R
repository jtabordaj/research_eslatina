source("./sui/dependencies.R")

readOutputs(2021, "energia")

merge_criteria <- c("Departamento", "Municipio")
df <- left_join(get(paste(variable,year,"_1", sep = "")), get(paste(variable,year,"_2", sep = "")), by = merge_criteria )

frameNames <- c("vcon", "tcon", "sus", "ptar", "pfac", "pcon", "fac")

for(i in 3:12){
    df <- left_join(df, get(paste(variable,year,"_",i, sep = "")), by = merge_criteria )
}