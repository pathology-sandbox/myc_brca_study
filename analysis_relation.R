library(psych)
library(dplyr)

# GET DATA 
source('get_data.R') # get tidy data via script
mut_vs_amp <- df_list[[1]]
muts_brca1 <- df_list[[2]]
muts_brca2 <- df_list[[3]]
demog <- df_list[[4]]
clinical <- df_list[[5]]
rm('df_list') # Remove df_list obtained from source('get_data.R')

df <- left_join(mut_vs_amp, clinical, by = 'sample_id')

intersect(df$sample_id, clinical$sample_id)
df$age<-df$AgeAtDiagnosis..yrs.
df$AgeAtDiagnosis..yrs. <- NULL

df$age_dicot <- cut(
  df$age, breaks = c(-Inf, 60, +Inf), 
  labels = c('<=60','>60'))
df$myc_cat <- as.factor(df$myc_cat)
df$brca1_cat <- as.factor(df$brca1_cat)
df$brca2_cat <- as.factor(df$brca2_cat)
df$sex <- NULL
df$race <- NULL

names(df)
numerical <- c('age', 'OverallSurvival.mos.', 'ProgressionFreeSurvival..mos..', 'PlatinumFreeInterval..mos..'
               )
categorical <- c('sample_id', 'MYC', 'BRCA1', 'BRCA2',
                 'myc_cat', 'brca1_cat', 'brca2_cat', 'brca_mutated', 'age_dicot',
                 'VITALSTATUS', 'TUMORSTAGE', 'TUMORGRADE', 'TUMORRESIDUALDISEASE',
                 'PRIMARYTHERAPYOUTCOMESUCCESS', 'PERSONNEOPLASMCANCERSTATUS',
                 'ProgressionFreeStatus', 'PlatinumStatus'
                 )
df[numerical] <- sapply(df[numerical], function(x) {as.numeric(unlist(x))}) 
sapply(df[numerical], typeof)
df[ categorical] <- sapply(df[categorical], factor) 
sapply(df[categorical], typeof)


# DATA EXPLORATION AND DESCRIPTIVE STATS

head(df[,c(3,6,4,7)])
head(df[numerical])
head(df[categorical])

summary(df[numerical])
describe(df[,numerical])

categ_data_summary <- function(in_col){
  counts <- as.matrix(table(in_col))
  proportions <- as.matrix(round(table(in_col)/length(in_col), 2))
  rmatch <- match(rownames(counts), rownames(proportions) )
  cat_data <- as.data.frame(cbind( counts, proportions[rmatch,] ))
  colnames(cat_data) <- c('counts', 'proportions')
  return(cat_data)
}
categorical_summary <- c("MYC", "myc_cat", "brca1_cat", "brca2_cat", "age_dicot")
cat_data <- lapply(df[categorical_summary], categ_data_summary)
