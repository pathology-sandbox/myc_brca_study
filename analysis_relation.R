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
df$age_dicot <- cut(df$age, breaks = c(-Inf, 60, +Inf), labels = c('<=60','>60'))
df$myc_cat <- as.factor(df$myc_cat)
df$brca1_cat <- as.factor(df$brca1_cat)
df$brca2_cat <- as.factor(df$brca2_cat)
