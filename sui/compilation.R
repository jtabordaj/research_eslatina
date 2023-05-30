source("./sui/dependencies.R")

readFiles(2021, 1, 12, "energia", "pcon")

allDataFrames <- names(which(unlist(eapply(.GlobalEnv, is.data.frame))))
dflist <- list()

for(df in allDataFrames){
    dflist[[df]] <- get(df)
}




# Define a custom merge function to sum numeric rows
merge_and_sum <- function(x, y) {
  merged <- merge(x, y, by = c("row_names", "col_names"), all = TRUE)
  merged[, -c(1, 2)] <- rowSums(merged[, -c(1, 2)], na.rm = TRUE)
  return(merged)
}

# Merge the data frames and sum the numeric rows
merged_df <- Reduce(merge_and_sum, df_list)

# Print the merged data frame
print(merged_df)