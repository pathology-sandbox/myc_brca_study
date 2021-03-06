library(psych)
library(dplyr)
library(rms)

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
df$brca_mutated <- as.factor(df$brca_mutated)
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

categorical_summary <- c(
  'MYC', 'myc_cat', 'brca1_cat', 'brca2_cat', 'brca_mutated', 'age_dicot',
  'VITALSTATUS', 'TUMORSTAGE', 'TUMORGRADE', 'TUMORRESIDUALDISEASE',
  'PRIMARYTHERAPYOUTCOMESUCCESS', 'PERSONNEOPLASMCANCERSTATUS',
  'ProgressionFreeStatus', 'PlatinumStatus')
cat_data <- lapply(df[categorical_summary], categ_data_summary)

# Models
df <- within(df, brca1 <- relevel(brca1_cat, ref = 'NOT_MUTATED'))
df <- within(df, brca2 <- relevel(brca2, ref = 'NOT_MUTATED'))
df <- within(df, myc_cat <- relevel(myc_cat, ref = 'AMP'))
(round(table(df$brca1_cat , df$myc_cat)/316,2)*100)

## BRCA1
brca1_myc_uni <- lrm(
  brca1_cat ~ myc_cat,
  data = df,
  na.action = na.delete)

brca1_myc_multi <- lrm(
  brca1_cat ~ myc_cat + age_dicot + TUMORSTAGE,
  data = df,
  na.action = na.delete)

## BRCA1
brca2_myc_uni <- lrm(
  brca2_cat ~ myc_cat,
  data = df,
  na.action = na.delete)

brca2_myc_multi <- lrm(
  brca2_cat ~ myc_cat + age_dicot + TUMORSTAGE,
  data = df,
  na.action = na.delete)
