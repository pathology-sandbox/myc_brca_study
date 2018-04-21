library(psych)
library(dplyr)


source('get_data.R') # get tidy data via script
# Variables: 
mut_vs_amp <- df_list[[1]]
muts_brca1 <- df_list[[2]]
muts_brca2 <- df_list[[3]]
demog <- df_list[[4]]

rm("df_list")

df <- left_join(mut_vs_amp, demog, by = 'sample_id')
