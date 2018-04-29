library(psych)
library(dplyr)
library(rms)

# GET DATA 
source('get_data_tcga.R') # get tidy data via script
mut_vs_amp <- df_list[[1]]
muts_brca1 <- df_list[[2]]
muts_brca2 <- df_list[[3]]
demog <- df_list[[4]]
clinical <- df_list[[5]]
rm('df_list') # Remove df_list obtained from source('get_data.R')

df_tcga <- left_join(mut_vs_amp, clinical, by = 'sample_id')



intersect(df_tcga$sample_id, clinical$sample_id)
df_tcga$age<-df_tcga$AgeAtDiagnosis..yrs.
df_tcga$AgeAtDiagnosis..yrs. <- NULL

df_tcga$age_dicot <- cut(
  df_tcga$age, breaks = c(-Inf, 60, +Inf), 
  labels = c('<=60','>60'))
df_tcga$myc_cat <- as.factor(df_tcga$myc_cat)
df_tcga$brca1_cat <- as.factor(df_tcga$brca1_cat)
df_tcga$brca2_cat <- as.factor(df_tcga$brca2_cat)
df_tcga$brca_mutated <- as.factor(df_tcga$brca_mutated)
df_tcga$sex <- NULL
df_tcga$race <- NULL

names(df_tcga)
numerical <- c('age', 'OverallSurvival.mos.', 'ProgressionFreeSurvival..mos..', 'PlatinumFreeInterval..mos..'
               )
categorical <- c('sample_id', 'MYC', 'BRCA1', 'BRCA2',
                 'myc_cat', 'brca1_cat', 'brca2_cat', 'brca_mutated', 'age_dicot',
                 'VITALSTATUS', 'TUMORSTAGE', 'TUMORGRADE', 'TUMORRESIDUALDISEASE',
                 'PRIMARYTHERAPYOUTCOMESUCCESS', 'PERSONNEOPLASMCANCERSTATUS',
                 'ProgressionFreeStatus', 'PlatinumStatus'
                 )
df_tcga[numerical] <- sapply(df_tcga[numerical], function(x) {as.numeric(unlist(x))}) 
sapply(df_tcga[numerical], typeof)
# df_tcga[ categorical] <- sapply(df_tcga[categorical], function(x) {as.factor(x)}) 
sapply(df_tcga[categorical], typeof)


# DATA EXPLORATION AND DESCRIPTIVE STATS
head(df_tcga[,c(3,6,4,7)])
head(df_tcga[numerical])
head(df_tcga[categorical])

summary(df_tcga[numerical])
describe(df_tcga[,numerical])

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
cat_data <- lapply(df_tcga[categorical_summary], categ_data_summary)

# Models
df_tcga <- within(df_tcga, brca1 <- relevel(brca1_cat, ref = 'NOT_MUTATED'))
df_tcga <- within(df_tcga, brca2 <- relevel(brca2_cat, ref = 'NOT_MUTATED'))
df_tcga <- within(df_tcga, myc_cat <- relevel(myc_cat, ref = 'AMP'))
(round(table(df_tcga$brca2_cat , df_tcga$myc_cat)/316,2)*100)
table(df_tcga$brca2_cat, df_tcga$myc_cat)

## BRCA1
brca1_myc_uni <- lrm(
  brca1_cat ~ myc_cat,
  data = df_tcga,
  na.action = na.delete)

brca1_myc_multi <- lrm(
  brca1_cat ~ myc_cat + age_dicot + TUMORSTAGE,
  data = df_tcga,
  na.action = na.delete)

## BRCA1
brca2_myc_uni <- lrm(
  brca2_cat ~ myc_cat,
  data = df_tcga,
  na.action = na.delete)

brca2_myc_multi <- lrm(
  brca2_cat ~ myc_cat + age_dicot + TUMORSTAGE,
  data = df_tcga,
  na.action = na.delete)
