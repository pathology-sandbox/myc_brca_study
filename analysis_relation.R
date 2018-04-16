

source('get_data.R') # get tidy data via script
# Variables: 
mut_vs_amp <- df_list[[1]]
muts_brca1 <- df_list[[2]]
muts_brca2 <- df_list[[3]]
demog <- df_list[[4]]
amp_my <- df_list[[5]]
amp_brca1 <- df_list[[6]]
amp_brca2 <- df_list[[7]]
rm("df_list")

